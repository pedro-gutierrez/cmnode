(function() {
    const state = {}
    
    function tc(fun) {
        const t1 = new Date();
        const r = fun();
        const t2 = new Date();
        return { millis: t2.getMilliseconds() - t1.getMilliseconds(),
                 res: r };
    }

    function error(spec, data, reason) {
        return {err: {spec, data, reason}};
    }
    function fmt(pattern, args) {
        var argNum = 0;
        return pattern.replace(/~a/gi, function(match) {
            var curArgNum, prop = null;
            curArgNum = argNum;
            argNum++;
            var result = curArgNum >= args.length ? "" : prop ? args[curArgNum][prop] || "" : args[curArgNum];
            return result;
        });
    };

    function encodeEntries(spec, data) {
        function _(keys, v) {
            if (!keys.length) return {value: v};
            var k = keys.shift();
            const { err, value } = encode(spec[k], data);
            if (err) return error(spec[k], data, err);
            v[k] = value;
            return _(keys, v);
        }
        return _(Object.keys(spec), {});
    }

    function encodeKey(spec, data) {
        if (!data) return error(spec, data, "no_data");
        switch(typeof(spec)) {
            case 'string':
                if (!data.hasOwnProperty(spec)) return error(spec, data, "missing_key");
                return { value: data[spec]};
            case 'object':
                if (spec.in) {
                    var {err, value} = encodeKey(spec.in, data);
                    if (err) return error(spec, data, err);
                    return encodeKey({ key: spec.key }, value)
                } else {
                    switch (typeof(spec.key)) {
                        case 'object' :
                            var {err, value} = encode(spec.key, data);
                            if (err) return error(spec, data, err);
                            return encodeKey({ key: value }, data);
                        case 'string' :
                            if (!data.hasOwnProperty(spec.key)) return error(spec, data, "missing_key");
                            return { value: data[spec.key]};
                        default:
                            return error(spec, data, "key_spec_not_supported");
                    }
                }
            default:
                return error(spec, data, "key_spec_not_supported");
        }
    }

    function encodeList(items, data) {
        function _(items, v) {
            if (!items.length) return { value: v };
            var item = items.shift();
            const { err, value } = encode(item, data);
            if (err) return error(item, data, err);
            v.push(value);
            return _(items, v);
        }
        return _(items.slice(0), []);
    }

    function encodeFormat(spec, data) {
        const { pattern, params } = spec.format;
        const { err, value } = encodeList(params, data);
        return err ? error(spec, data, err) : {value: fmt( pattern, value)};
    }

    function encodeMaybe(spec, data) {
        const { err, value } = encode(spec.maybe, data);
        return {value};
    }

    function encodeEqual(spec, data) {
        function _(rem, v) {
            if (!rem.length) return { value: true };
            const s = rem.shift();
            const { err, value } = encode(s, data);
            if (err) return error(spec, data, err);
            return (v && value != v) ? { value: false } : _(rem, value);
        }
        return _(spec.equal.slice(0));
    }

    function encodeEither(spec, data) {
        function _(i) {
            if (i == spec.either.length) return error(spec, data, "no_condition_matched")
            var s = spec.either[i];
            var { err, value } = encode(s.when, data);
            if (err) return error(spec, data, err);
            if (!value) return _(i+1);
            var { err, value } = encode(s.then, data);
            if (err) return error(spec, data, err);
            return { value };
        }
        return _(0);
    }

    function encodeUsingEncoder(spec, data) {
        var enc = state.app.encoders[spec.encoder];
        if (!enc) return error(spec, data, "no_such_encoder");
        return encode(enc, data);
    }

    function encodeTimestamp(spec, data) {
        if (spec.timestamp.format) {
            switch(spec.timestamp.format) {
                case "human":
                    var {err, value} = encode(spec.timestamp.value, data);
                    if (err) return error(spec, data, err);
                    return { value: moment(value).fromNow() };
            }
        }
        return error(spec, data, "unsupported_timestamp_spec");
    }


    function encode(spec, data) {
        switch(typeof(spec)) {
            case "object":
                if (Array.isArray(spec)) return encodeList(spec, data);
                if (spec.object) return encodeEntries(spec.object, data);
                if (spec.key) return encodeKey(spec, data);
                if (spec.format) return encodeFormat(spec, data);
                if (spec.maybe) return encodeMaybe(spec, data);
                if (spec.equal) return encodeEqual(spec, data);
                if (spec.either) return encodeEither(spec, data);
                if (spec.encoder) return encodeUsingEncoder(spec, data);
                if (spec.timestamp) return encodeTimestamp(spec, data);
                if (!Object.keys(spec).length) return {value: {}};
                return error(spec, data, 'encoder_not_supported');
            default: 
                return { value: spec };
        }
    }

    function decodeObject(spec, data, ctx) {
        if (!data || typeof(data) != 'object') return error(spec, data, "no_match");
        function _(keys, out) {
            if (!keys.length) return { decoded: out};
            var k = keys.shift();
            var spec2 = spec[k];
            const {err, decoded} = decode(spec2, data[k], ctx);
            if (err) return error(spec, data, err);
            out[k] = decoded;
            return _(keys, out);
        }
        return _(Object.keys(spec), {});
    }


    function decodeAny(spec, data, ctx) {
        switch (spec) {
            case "text":
                if (typeof(data) === 'string') return {decoded: data};
            case "number":
                if (typeof(data) === 'number') return {decoded: data};
            case "boolean":
                if (typeof(data) === 'boolean') return {decoded: data};
            case "list": 
                if (Array.isArray(data)) return {decoded: data};
            default: 
                return error(spec, data, "no_match");
        }
    }

    function decodeObjects(spec, data, ctx) {
        function _(rem, out) {
            if (!rem.length) return {decoded: out};
            var item = rem.shift();
            const {err, decoded} = decodeObject(spec, item, ctx);
            if (err) return error(spec, data, err);
            out.push(decoded);
            return _(rem, out);
        }
        return _(data.slice(0), []);
    }

    function decodeList(spec, data, ctx) {
        if (!Array.isArray(data)) return error(spec, data, "no_match");
        if (Array.isArray(spec)) {
            return error(spec, data, "decoder_not_supported");
        } else {
            if (spec.object) return decodeObjects(spec.object, data, ctx);
            return error(spec, data, "decoder_not_supported");
        }
    }
    
    function decodeOne(spec, data, ctx) {
        function _(i) {
            if (i == spec.length) return error(spec, data, "no_match")
            var s = spec[i];
            var { err, decoded } = decode(s, data, ctx);
            if (err) return _(i+1);
            return { decoded };
        }
        return _(0);
    }   

    function decode(spec, data, ctx) {
        switch(typeof(spec)) {
            case "object":
                if (Array.isArray(spec)) return error(spec, data, "decoder_not_supported");
                if (spec.object) return decodeObject(spec.object, data, ctx);
                if (spec.any) return decodeAny(spec.any, data, ctx);
                if (spec.list) return decodeList(spec.list, data, ctx);
                if (spec.one_of) return decodeOne(spec.one_of, data, ctx);
            default :
                return (spec === data) ? { decoded: data } : error(spec, data, "no_match");
        }
    }

    function tryDecoders(data, decoders) {
        function _(keys) {
            if (!keys.length) return error(null, data, "all_decoders_failed");
            var k = keys.shift();
            var spec = decoders[k];
            const {err, decoded} = decode(spec, data, state.model)
            if (err) return _(keys);
            return {decoded: {msg: k, data: decoded}};
        }
        return _(Object.keys(decoders));
    }

    function selectUpdate(msg, update, data, model, next) {
        var clauses = update[msg];
        if (!clauses) return next({
            error: "no_update",
            msg: msg,
            update: update
        });

        function _(i) {
            if (i == clauses.length) return next({
                error: "no_condition",
                msg: msg,
                update: update,
                input: input
            });
            var c = clauses[i]
            const { err, value } = encode(c.condition, model);
            if (err) return error(c.condition, model, err);
            return value ? next(null, c) : _(i+1);
        }
        _(0);
    }


    function app(next) {
        fetch('/js/app.js')
            .then((mod) => {
                return mod.json();
            })
            .then(next)
            .catch((err) => {
                console.error("app error", err);
            });
    };

    function effects(app, next) {
        const names = Object.keys(app.effects);
        Promise.all(names.map(function(n) {
            return import('/js/' + app.effects[n].class  + '.js');
        })).then((mods) => {
            const out = {};
            for (var i=0; i<names.length; i++) {
                const n = names[i];
                const send = mods[i].default(n, app.effects[n].settings, {
                    encode,
                    decode,
                    update,
                    tc
                });
                if (send instanceof Function) {
                    out[n] = send;
                } else {
                    console.warn('effect ' + n + ' of type ' + app.effects[n].class
                        + ' is not returning a send function', send);
                }
            }
            next(out);
        });
    }

    function selectEncoder(enc, encoders, next) {
        if (!enc) return next(null);
        return encoders[enc] ? next(null, encoders[enc]) : next({
            error: "no_such_encoder",
            name: enc,
            encoders: encoders
        });
    }

    function applyCmds(encoders, effects, cmds, m2) {
        cmds.forEach((cmd) => {
            const { effect, encoder } = cmd;
            const eff = effects[effect];
            if (!eff) {
                console.error("No such effect", {
                    effect: effect,
                    cmd: cmd
                });
            } else {
                selectEncoder(encoder, encoders, (err, enc) => {
                    if (err) {
                        console.error("No such encoder", err);
                    } else eff(encoders, enc, Object.assign({}, m2));
                });
            }
        });
    }

    function init(app) {
        const { init } = app;
        const { model, cmds } = init;
        const { err, value } = encode(model, {});
        if (err) {
            console.error("(init) cannot encode model", err);
        } else {
            state.model = value;
            applyCmds(app.encoders, state.effects, cmds, state.model);
        }
    }

    function update(ev) {
        const { decoders, update } = state.app;
        var {err, decoded } = tryDecoders(ev, decoders);
        if (decoded) {
            const { msg, data } = decoded;
            selectUpdate(msg, update, data, state.model, (err, spec) => {
                if (err) {
                    console.error("No update spec", msg, err);
                } else {
                    const { encoders, effects } = state.app;
                    if (!spec.model) {
                        applyCmds(
                            encoders,
                            state.effects,
                            spec.cmds,
                            state.model);
                    } else {
                        const { err, value } = encode(spec.model, data);
                        if (err) {
                            console.error("Unable to update model", msg, spec, data, err);
                        } else {
                            Object.assign(state.model, value);
                            applyCmds(
                                encoders,
                                state.effects,
                                spec.cmds,
                                state.model);
                        }
                    }
                }
            });
        } else console.error("Decode error", ev, err);
    }

    app(function(app) {
        state.app = app;
        console.log(app);
        effects(app, (effs) => {
            state.effects = effs;
            init(app);
        });
    });

})();
