export default (appUrl, appEffects) => {
    const config = (window.elementary || {})
    const state = {}

    function sleep(sleepDuration) {
        var now = new Date().getTime();
        while(new Date().getTime() < now + sleepDuration){ /* do nothing */ } 
    }

    function elapsed(t1, t2) {
        return t2.getTime() - t1.getTime();
    }

    function tc(fun) {
        const t1 = new Date();
        const r = fun();
        const t2 = new Date();
        return { millis: elapsed(t1, t2), res: r };
    }

    function typeOf(data) {
        if (!data) return "undefined";
        var t = typeof(data);
        if (t === 'object') {
            return (Array.isArray(data)) ? "list" : "object";
        } else return (t === 'string') ? "text" : t;
    }

    function error(spec, data, reason) {
        return {err: {spec, data, reason}};
    }
    function fmt(pattern, args) {
        var argNum = 0;
        return pattern.replace(/~a|~s/gi, function(match) {
            var curArgNum, prop = null;
            curArgNum = argNum;
            argNum++;
            var result = curArgNum >= args.length ? "" : prop ? args[curArgNum][prop] || "" : args[curArgNum];
            return result;
        });
    };

    function flatten(arr) {
        return [].concat(...arr)
    }

    function encodeObject(spec, data, ctx, out) {
        for (var k in spec.object) {
            if (spec.object.hasOwnProperty(k)) {
                const { err, value } = encode(spec.object[k], data, (ctx || {})[k]);
                if (err) return error(spec.object[k], data, err);
                out[k] = value;
            }
        }
        return { value: out };
    }

    function encodeNewObject(spec, data, ctx) {
        return encodeObject(spec, data, ctx, {});
    }
    
    function encodeKey(spec, data, ctx) {
        if (!data) return error(spec, data, "no_data");
        switch(typeof(spec)) {
            case 'string':
                if (!data.hasOwnProperty(spec)) {
                    return error(spec, data, "missing_key");
                }
                return { value: data[spec]};
            case 'object':
                if (spec.in) {
                    var {err, value} = encodeKey(spec.in, data, ctx);
                    if (err) {
                        if (spec.hasOwnProperty("default")) {
                            var {err, value} = encode(spec['default'], data, ctx);
                            if (err) return error(spec, data, err);
                            return {value};
                        }
                        return error(spec, data, err);
                    }
                    var inCtx = value;
                    switch (typeof(spec.key)) {
                        case 'object':
                            var {err, value} = encode(spec.key, data, ctx);
                            if (err) return error(spec, data, err);
                            return encodeKey({key: value}, inCtx, ctx);
                        case 'string':
                            if (!inCtx.hasOwnProperty(spec.key)) {
                                if (!spec.hasOwnProperty("default"))  
                                    return error(spec, inCtx, "missing_key");
                                var {err, value} = encode(spec['default'], data, ctx);
                                if (err) return error(spec, data, err);
                                return {value};
                            }
                            return { value: inCtx[spec.key]};
                        
                        default:
                            return error(spec, data, "key_spec_not_supported");

                    }
                } else {
                    switch (typeof(spec.key)) {
                        case 'object' :
                            var {err, value} = encode(spec.key, data, ctx);
                            if (err) return error(spec, data, err);
                            return encodeKey({key: value}, data, ctx);
                        case 'string' :
                            if (!data.hasOwnProperty(spec.key)) {
                                if (!spec.hasOwnProperty("default")) {   
                                    return error(spec, data, "missing_key");
                                }
                                var {err, value} = encode(spec['default'], data, ctx);
                                if (err) return error(spec, data, err);
                                return {value};
                            }
                            return { value: data[spec.key]};
                        default:
                            return error(spec, data, "key_spec_not_supported");
                    }
                }
            default:
                return error(spec, data, "key_spec_not_supported");
        }
    }

    function encodeList(items, data, ctx) {
        function _(items, v) {
            if (!items.length) return { value: v };
            var item = items.shift();
            const { err, value } = encode(item, data, ctx);
            if (err) return error(item, data, err);
            v.push(value);
            return _(items, v);
        }
        return _(items.slice(0), []);
    }
    
    function encodeByAppending(spec, data, ctx) {
        if (!ctx) return error(spec, ctx, "no_data");
        switch(typeof(ctx)) {
            case 'object':
                if (Array.isArray(ctx)) {
                    var {err, value} = encode(spec.by_appending, data, ctx);
                    if (err) return error(spec, data, err);
                    ctx.push(value);
                    return { value: ctx };
                }
        }

        return error(spec, ctx, "by_adding spec not supported");
    }
    
    function encodeByRemoving(spec, data, ctx) {
        if (!ctx) return error(spec, ctx, "no_data");
        switch(typeof(ctx)) {
            case 'object':
                if (Array.isArray(ctx)) {
                    for (var i=0; i<ctx.length; i++) {
                        var item = ctx[i];
                        var {err, decoded} = decode(spec.by_removing, item, data);
                        if (!err) {
                            ctx.splice(i, 1);
                        }
                    }
                    return { value: ctx};
                }
        }
        return error(spec, ctx, "by_removing spec not supported");
    }

    function encodeByReplacing(spec, data, ctx) {
        if (!ctx) return error(spec, ctx, "no_data");
        switch(typeof(ctx)) {
            case 'object':
                if (Array.isArray(ctx)) {
                    var itemSpec = spec.by_replacing.items;
                    var encodeSpec = spec.by_replacing.with;
                    var out = [];
                    for (var i=0; i<ctx.length; i++) {
                        var item = ctx[i];
                        var {err, decoded} = decode(itemSpec, item, data);
                        if (err) {
                            out[i] = item;
                        } else {
                            var {err, value} = encode(encodeSpec, {item, data}, item);
                            if (err) return error(spec, data, err);
                            out[i] = value;
                        }
                    }
                    return {value: out};
                }

                return encodeObject(spec.by_replacing, data, ctx, ctx);
        }
        
        return error(spec, ctx, "by_replacing spec not supported");
    }
    
    


    function encodeKeyPath(spec, data, ctx) {
        var parts = spec.key_path.split(".");
        var d = data;
        for (var i=0; i<parts.length; i++) {
            var p = parts[i];
            if (!d.hasOwnProperty(p)) return error(p, d, "missing_key");
            d = d[p];
        }
        return { value: d };
    }

    function encodeI18n(spec, data, ctx) {
        var {err, value} = encode(spec.lang, data, ctx);
        if (err) return error(spec, data, err);
        var lang = value;
        var {err, value} = encode(spec.i18n, data, ctx);
        if (err) return error(spec, data, err);
        var keyPath = value;
        var {err, value} = encodeKeyPath({ key_path: keyPath + "." + lang }, data, ctx);
        if (err) return { value: "??" + keyPath + "." + lang + "??" };
        return { value };
    }

    function encodeFormatText(spec, data, ctx) {
        var {pattern, params} = spec.format_text;
        var {err, value} = encode(pattern, data, ctx);
        if (err) return error(spec, data, err);
        var pattern = value;
        var {err, value} = encodeList(params, data, ctx);
        return err ? error(spec, data, err) : {value: fmt( pattern, value)};
    }

    
    function encodeFormatDate(spec, data, ctx) {
        var {pattern, date} = spec.format_date;
        var {err, value} = encode(pattern, data, ctx);
        if (err) return error(spec, data, err);
        var pattern = value;
        var {err, value} = encode(date, data, ctx);
        if (err) return error(spec, data, err);
        switch (pattern) {
            case "relative":
            case "human":
                return { value: moment(value).fromNow() };
            default:
                return { value: moment(value).format(pattern) };
        }
    }


    function encodeMaybe(spec, data, ctx) {
        const { err, value } = encode(spec.maybe, data, ctx);
        return {value};
    }

    function encodeEqual(spec, data, ctx) {
        if (!spec.equal.length) return {value: false};
        var {err, value}= encode(spec.equal[0], data, ctx);
        if (err) return error(spec, data, err);
        var expected = value;
        for (var i=1; i<spec.equal.length; i++) {
            var s = spec.equal[i];
            var { err, value}= encode(s, data, ctx);
            if (err) return error(spec, data, err);
            if (value != expected) return { value: false};
        }
        
        return {value: true};
    }
    
    function encodeAnd(spec, data, ctx) {
        if (!spec.and) return {value: false};
        for (var i=0; i<spec.and.length; i++) {
            var s = spec.and[i];
            var {err, value}= encode(s, data, ctx);
            if (err) return error(spec, data, err);
            if (!value) return {value: false}
        }
        return {value: true};
    }
    
    function encodeOr(spec, data, ctx) {
        if (!spec.or) return {value: false};
        for (var i=0; i<spec.or.length; i++) {
            var s = spec.or[i];
            var {err, value}= encode(s, data, ctx);
            if (err) return error(spec, data, err);
            if (value) return {value: true}
        }
        return {value: false};
    }

    function encodeIsSet(spec, data, ctx) {
        var {err, value} = encode(spec.is_set, data, ctx);
        if (err) return error(spec, data, err);
        var v = data[value];
        if (!v) return {value: false};
        switch (typeof(v)) {
            case 'string':
                return v.length ? { value: true }: {value: false};
        }
        return {value: true}
    }
    
    function encodeNot(spec, data, ctx) {
        var {err, value} = encode(spec.not, data, ctx);
        if (err) return error(spec, data, err);
        if (!typeof(value) === 'boolean') return error(spec, data, {
            value: value,
            reason: "not_a_boolean"
        });
        return { value: !value };
    }

    function encodeEither(spec, data, ctx) {
        if (!spec.either.length) return error(spec, data, "not_conditions");
        for (var i=0; i<spec.either.length; i++) {
            var s = spec.either[i];
            var { err, value } = encode(s.when, data, ctx);
            if (err) return error(spec, data, err);
            if (value) {
                var { err, value } = encode(s.then, data, ctx);
                if (err) return error(spec, data, err);
                return { value };
            }
        }
        return error(spec, data, "no_condition_matched");
    }

    function encodeUsingEncoder(spec, data, ctx) {
        var {err, value} = encode(spec.encoder, data, ctx);
        if (err) return error(spec, data, err);
        var encName = value;
        var enc = state.app.encoders[encName];
        if (!enc) return error(spec, data, "no_such_encoder: " + encName);
        return encode(enc, data, ctx);
    }
    
    function encodeUsingExpression(spec, data, ctx) {
        var {err, value} = encode(spec.encode, data, ctx);
        if (err) return error(spec, data, err);
        var s0 = value;
        if (spec.as) {
            var {err, value} = encode(spec.as, data, ctx);
            var s = {}
            s[value] = s0;
        } else {
            var s = s0;
        }
        return encode(spec["with"], s, ctx);
    }

    function encodeTimestamp(spec, data, ctx) {
        return encode({ 
            format_date: {
                pattern: spec.timestamp.format,
                date: spec.timestamp.value
            }
        }, data, ctx);
    }

    function encodeText(spec, data, ctx) {
        var {err, value} = encode(spec.text, data, ctx);
        if (err) return error(spec, data, err);
        //return { value: new String(''+value) };
        return { value: ''+value};
    }

    function encodePercent(spec, data, ctx) {
        var {err, value} = encode(spec.percent.num, data, ctx);
        if (err) return error(spec, data, err);
        var num = value;
        var {err, value} = encode(spec.percent.den, data, ctx);
        if (err) return error(spec, data, err);
        var den = value;
        if (!den) return { value: 0 }; 
        return {value : Math.floor(num/den)*100};
        
    }

    function encodeAny(spec, data, ctx) {
        if (spec.any === 'object' && typeof(data) === 'object' && !Array.isArray(data)) {
            return {value: data};
        }

        return error(spec, data, "any spec not supported");
    }
    
    function encodeExpression(spec, data, ctx) {
        return {value: spec.expression};
    }
    
    function encodeEncoded(spec, data, ctx) {
        var {err, value} = encode(spec.encoded, data, ctx);
        if (err) return error(spec, data, err);
        return encode(value, data, ctx);
    }
        
    function encodeIterate(spec, data, ctx) {
        var {err, value} = encode(spec.iterate.source, data, ctx);
        if (err) return error(spec, data, err);
        var source = value;
        var out = [];
        var filterFn = spec.iterate.filter === 'none' ? 
            function(i, ctx) {
                return {value: true};
            } 
            :
            function(i, ctx) {
                return decode(spec.iterate.filter, i, ctx);
            };
        var itemFn = spec.iterate.as === 'none' ?
            function(i) {
                return i;
            } 
            :
            function(i) {
                var obj = {};
                obj[spec.iterate.as] = i;
                return obj;
            }

        for (var i=0; i<source.length; i++) {
            var {err, value} = filterFn(source[i], ctx);
            if (!err) {
                var item = itemFn(source[i]);
                var {err, value} = encode(spec.iterate.dest, item, ctx);
                if (err) return error(spec, data, err);
                out[i] = value;
            }
        }
        return {value: out};
    }

    function encodeMergedList(spec, data, ctx) {
        var lists = [];
        for (var i=0; i<spec.merged_list.length; i++) {
            var {err, value} = encode(spec.merged_list[i], data, ctx);
            if (err) return error(spec, data, err);
            lists[i] = value;
        }
        return {value: flatten(lists)}
    }

    function encodeMergedObject(spec, data, ctx) {
        var objs = [];
        for (var i=0; i<spec.merge.length; i++) {
            var {err, value} = encode(spec.merge[i], data, ctx);
            if (err) return error(spec, data, err);
            objs[i] = value;
        }
        return {value: Object.assign({}, ...objs)}
    }

    function encodePrettify(spec, data, ctx) {
        var {err, value} = encode(spec.prettify, data, ctx);
        if (err) return error(spec, data, err);
        return {value: JSON.stringify(value, null, 2)};
    }

    function encodePercent(spec, data, ctx) {
        var {err, value} = encode(spec.percent.den, data, ctx);
        if (err) return error(spec, data, err);
        if (!value) return error(spec, data, "denominator is zero");
        var den = value;
        var {err, value} = encode(spec.percent.num, data, ctx);
        if (err) return error(spec, data, err);
        return {value: Math.round(100*value/den)};
    }

    function encodeSum(spec, data, ctx) {
        var {err, value} = encodeList(spec.sum, data, ctx);
        if (err) return error(spec, data, err);
        if (!value || !value.length) return {value: 0};
        var sum = 0;
        for (var i=0; i<value.length; i++) {
            var item = value[i];
            if (typeof(item) != 'number') return error(spec, item, "not_a_number");
            sum += item;
        }
        return {value: sum};
    }

    function encodeDivide(spec, data, ctx) {
        var {err, value} = encodeList(spec.divide, data, ctx);
        if (err) return error(spec, data, err);
        if (!value || value.length < 2) return error(spec, item, "not_enough_arguments");
        var num = value[0];
        for (var i=1; i<value.length; i++) {
            if (typeof(value[i]) != 'number') return error(spec, value, "not_a_number");
            if (!value[i]) return error(spec, value, "division_by_zero");
            num = num / value[i];
        }
        if (!spec.hasOwnProperty("decimals")) return {value: num};
        return {value: num.toFixed(spec.decimals)};
    }

    var sizeEncoders = {
        "undefined": (d) => { return 0},
        "list": (d) => { return d.length },
        "object": (d) => { return Object.keys(d).length },
        "string": (d) => { return d.length },
        "number": (d) => { return d.number },
        "boolean": (d) => { return 0 }
    }

    function encodeSizeOf(spec, data, ctx) {
        var {err, value} = encode(spec.size_of, data, ctx);
        if (err) return error(spec, data, err);
        var e = sizeEncoders[typeOf(value)];
        if (!e) return error(spec, value, "cannot_encode_size");
        var s = e(value);
        return {value: s};
    }

    function encodeLowerCase(spec, data, ctx) {
        var {err, value} = encode(spec.lowercase, data, ctx);
        if (err) return error(spec, data, err);
        switch (typeof(value)) {
            case "string": 
                return {value: value.toLowerCase()};
            default:
                return error(spec, value, "not_a_string");
        }
    }
    
    function encodeUpperCase(spec, data, ctx) {
        var {err, value} = encode(spec.uppercase, data, ctx);
        if (err) return error(spec, data, err);
        switch (typeof(value)) {
            case "string": 
                return {value: value.toUpperCase()};
            default:
                return error(spec, value, "not_a_string");
        }
    }

    function encodeOneOf(spec, data, ctx) {
        for (var i=0; i<spec.one_of.length; i++) {
            var {err, value} = encode(spec.one_of[i], data, ctx);
            if (!err && value) {
                return {value: value}
            }
        }
        return error(spec, data, "no_valid_non_null_expression_found");
    }


    function encodeCmd(spec, data, ctx) {
        var {err, value} = encode(spec.effect, data, ctx);
        if (err) return error(spec, data, err);
        var cmd = { effect: value };
        if (spec.encoder) {
            var {err, value} = encode(spec.encoder, data, ctx);
            if (err) return error(spec, data, err);
            cmd.encoder = value;
        }
        return {value: cmd};
    }

    function encodeSplit(spec, data, ctx) {
        var {err, value} = encode(spec.using, data, ctx);
        if (err) return error(spec, data, err);
        var sep = value; 
        var {err, value} = encode(spec.split, data, ctx);
        if (err) return error(spec, data, err);
        switch (typeof(value)) {
            case "string":
                return {value: value.split(sep)};
            default:
                return error(spec, value, "not_a_string");
        }
    }
    
    function encodeJoin(spec, data, ctx) {
        var {err, value} = encode(spec.using, data, ctx);
        if (err) return error(spec, data, err);
        var sep = value; 
        var {err, value} = encode(spec.join, data, ctx);
        if (err) return error(spec, data, err);
        if (!Array.isArray(value)) return error(spec, value, "not_a_list");
        return {value: value.join(sep)};
    }

    function encodeHead(spec, data, ctx) {
        var {err, value} = encode(spec.head, data, ctx);
        if (err) return error(spec, data, err);
        if (!Array.isArray(value)) return error(spec, value, "not_a_list");
        const [head, _] = value;
        return {value: head};

    }

    function encodeTail(spec, data, ctx) {
        var {err, value} = encode(spec.tail, data, ctx);
        if (err) return error(spec, data, err);
        if (!Array.isArray(value)) return error(spec, value, "not_a_list");
        const [_, ...tail] = value;
        return {value: tail};
    }
    
    function encodeLast(spec, data, ctx) {
        var {err, value} = encode(spec.last, data, ctx);
        if (err) return error(spec, data, err);
        if (!Array.isArray(value)) return error(spec, value, "not_a_list");
        if (!value.length) return error(spec, value, "empty_list");
        return {value: value[value.length-1]};
    }

    function encodeChar(spec, data, ctx) {
        var {err, value} = encode(spec['char'], data, ctx);
        if (err) return error(spec, data, err);
        var c = value;
        var {err, value} = encode(spec['in'], data, ctx);
        if (err) return error(spec, data, err);
        return {value: value.charAt(c)};
    }           
    
    function encodeGreaterThan(spec, data, ctx) {
        if (spec.greater_than.length < 2) return error(spec, data, "not_enough_arguments");
        var {err, value}= encode(spec.greater_than[0], data, ctx);
        if (err) return error(spec, data, err);
        var v1  = value;
        var {err, value}= encode(spec.greater_than[1], data, ctx);
        if (err) return error(spec, data, err);
        return {value: v1>value};
    }

    function encodeLowerThan(spec, data, ctx) {
        if (spec.lower_than.length < 2) return error(spec, data, "not_enough_arguments");
        var {err, value}= encode(spec.lower_than[0], data, ctx);
        if (err) return error(spec, data, err);
        var v1  = value;
        var {err, value}= encode(spec.lower_than[1], data, ctx);
        if (err) return error(spec, data, err);
        return {value: v1<value};
    }

    function encodeCase(spec, data, ctx) {
        var {err, value} = encode(spec["case"], data, ctx);
        if (err) return error(spec, data, err);
        var clause = spec["of"][value];
        if (!clause) {
            return encode(spec.otherwise, data, ctx);
        } else {
            return encode(clause, data, ctx);
        }
    }
    
    function encode(spec, data, ctx) {
        //if (!spec) return error({}, data, "missing_encoder_spec");
        switch(typeof(spec)) {
            case "object":
                if (spec.hasOwnProperty("text")) return encodeText(spec, data, ctx);
                if (spec.hasOwnProperty("char")) return encodeChar(spec, data, ctx);
                if (Array.isArray(spec)) return encodeList(spec, data, ctx);
                if (spec.object) return encodeNewObject(spec, data, ctx);
                if (spec.hasOwnProperty("case")) return encodeCase(spec, data, ctx);
                if (spec.by_appending) return encodeByAppending(spec, data, ctx);
                if (spec.by_replacing) return encodeByReplacing(spec, data, ctx);
                if (spec.by_removing) return encodeByRemoving(spec, data, ctx);
                if (spec.key) return encodeKey(spec, data, ctx);
                if (spec.key_path) return encodeKeyPath(spec, data, ctx);
                if (spec.i18n) return encodeI18n(spec, data, ctx);
                if (spec.format_text) return encodeFormatText(spec, data, ctx);
                if (spec.format_date) return encodeFormatDate(spec, data, ctx);
                if (spec.timestamp) return encodeTimestamp(spec, data, ctx);
                if (spec.maybe) return encodeMaybe(spec, data, ctx);
                if (spec.equal) return encodeEqual(spec, data, ctx);
                if (spec.either) return encodeEither(spec, data, ctx);
		        if (spec.one_of) return encodeOneOf(spec, data, ctx);
                if (spec.effect) return encodeCmd(spec, data, ctx);
                if (spec.encoder) return encodeUsingEncoder(spec, data, ctx);
                if (spec.encode) return encodeUsingExpression(spec, data, ctx);
                if (spec.percent) return encodePercent(spec, data, ctx);
                if (spec.is_set) return encodeIsSet(spec, data, ctx);
                if (spec.not) return encodeNot(spec, data, ctx);
                if (spec.and) return encodeAnd(spec, data, ctx);
                if (spec.or) return encodeOr(spec, data, ctx);
                if (spec.any) return encodeAny(spec, data, ctx);
                if (spec.head) return encodeHead(spec, data, ctx);
                if (spec.tail) return encodeTail(spec, data, ctx);
                if (spec.last) return encodeLast(spec, data, ctx);
                if (spec.split) return encodeSplit(spec, data, ctx);
                if (spec.join) return encodeJoin(spec, data, ctx);
                if (spec.merged_list) return encodeMergedList(spec, data, ctx);
                if (spec.merge) return encodeMergedObject(spec, data, ctx);
                if (spec.iterate) return encodeIterate(spec, data, ctx);
                if (spec.expression) return encodeExpression(spec, data, ctx);
                if (spec.encoded) return encodeEncoded(spec, data, ctx);
                if (spec.prettify) return encodePrettify(spec, data, ctx);
                if (spec.percent) return encodePercent(spec, data, ctx);
                if (spec.divide) return encodeDivide(spec, data, ctx);
                if (spec.sum) return encodeSum(spec, data, ctx);
                if (spec.size_of) return encodeSizeOf(spec, data, ctx);
                if (spec.lowercase) return encodeLowerCase(spec, data, ctx);
                if (spec.uppercase) return encodeUpperCase(spec, data, ctx);
                if (spec.greater_than) return encodeGreaterThan(spec, data, ctx);
                if (spec.lower_than) return encodeLowerThan(spec, data, ctx);
                if (!Object.keys(spec).length) return {value: {}};
                console.warn("returning spec as default encoding value", spec);
                return { value: spec };
            case "string":
                return { value: spec };
            case "boolean":
                return { value: spec };
            case "number":
                return { value: spec };
            default:
                return error(spec, data, "encoder_not_supported");
        }
    }

    function decodeObject(spec, data, ctx) {
        if (!data || typeof(data) != 'object') return error(spec, data, "no_match");
        var out = {};
        for (var k in spec.object) {
            if (spec.object.hasOwnProperty(k)) {
                var {err, decoded} = decode(spec.object[k], data[k], ctx);
                if (err) return error(spec, data, err);
                out[k] = decoded;
            }
        }
        return {decoded: out};
    }
    
    function decodeOtherThan(spec, data, ctx) {
        var {err, value} = encode(spec.other_than, ctx);
        if (err) return error(spec, data, err);
        if (data != value) return {decoded: data};
        return error(spec, data, "no_match");
    }

    function no_match(spec, data) {
        return error(spec, data, "no_match");
    }
    
    function decodeType(spec, data, expected) {
        return typeof(data) === expected ?
            {decoded: data} : no_match(spec, data)
    }
    
    var anyConditions = {
        "text": (spec, data) => { 
            return decodeType(spec, data, 'string');
        },
        "number": (spec, data) => { 
            return decodeType(spec, data, "number");
        },
        "boolean": (spec, data) => { 
            return decodeType(spec, data, "boolean");
        },
        "object": (spec, data) => { 
            return decodeType(spec, data, "object");
        },
        "list": (spec, data) => { 
            return Array.isArray(data) ? 
                {decoded: data} : no_match(spec, data) 
        },
        "file": (spec, data) => {
            if (data && data.length && data.item) {
                var first = data.item(0);
                return {
                    decoded: {
                        name: first.name,
                        type: first.type,
                        size: first.size,
                        file: first
                    }
                };
            }
            return no_match(spec, data);
        }
    }

    function decodeAny(spec, data, ctx) {
        if (data == undefined || data == null) return error(spec, data, "no_data");
        var d = anyConditions[spec.any];
        if (!d) return error(spec, data, "unknown_any_condition");
        return d(spec, data);
    }

    function decodeListItems(spec, data, ctx) {
        var out = [];
        for (var i=0; i<data.length; i++) {
            var item = data[i];
            var { err, decoded } = decode(spec, item, ctx);
            if (err) return error(spec, data, err);
            out.push(decoded);
        }
        return { decoded: out};
    }

    function decodeList(spec, data, ctx) {
        if (!Array.isArray(data)) return error(spec, data, "no_match");
        if (Array.isArray(spec.list)) {
            return error(spec, data, "decoder_not_supported");
        } else return decodeListItems(spec.list, data, ctx);
    }
    
    function decodeOne(spec, data, ctx) {
        function _(i) {
            if (i == spec.one_of.length) return error(spec, data, "no_match")
            var s = spec.one_of[i];
            var { err, decoded } = decode(s, data, ctx);
            if (err) return _(i+1);
            return { decoded };
        }
        return _(0);
    } 
    
    
    function decodeJson(spec, data, ctx) {
        try {
            return { value: JSON.parse(spec.json)};
        } catch (e) {
            return { err: { json: spec.json, error: e} };
        }
    }
    
    function decodeSize(spec, data) {
        var {err, value} = encode(spec.size, data);
        if (err) return error(spec, data, err);
        switch (typeof(data)) {
            case 'object':
                if (Array.isArray(data) && data.length == value) return {decoded:data}; 
                if (Object.keys(data).length == value) return {decoded: data};
            case 'string':
                if (data && data.length == value) return {decoded: data};
        }
        return error(spec, data, "no_match");
    };

    function decodeKey(spec, data, ctx) {
        var {err, value} = encode(spec, ctx);
        if (err) return error(spec, data, err);
        return decode(value, data, ctx)
    }

    function decode(spec, data, ctx) {
        switch(typeof(spec)) {
            case "object":
                if (spec.hasOwnProperty("text")) return decode(spec.text, data, ctx);
                if (spec.key) return decodeKey(spec, data, ctx);
                if (Array.isArray(spec)) return error(spec, data, "decoder_not_supported");
                if (spec.object) return decodeObject(spec, data, ctx);
                if (spec.any) return decodeAny(spec, data, ctx);
                if (spec.list) return decodeList(spec, data, ctx);
                if (spec.other_than) return decodeOtherThan(spec, data, ctx);
                if (spec.one_of) return decodeOne(spec, data, ctx);
                if (spec.json) return decodeJson(spec, data, ctx);
                if (spec.size) return decodeSize(spec, data, ctx);
                return error(spec, data, "decoder_not_supported");
            default :
                return (spec === data) ? { decoded: data } : error(spec, data, "no_match");
        }
    }

    function tryDecoders(data, decoders) {
        if (!data.effect) return error(null, data, "no_effect_in_data");
        var decs = decoders[data.effect];
        if (!decs || !decs.length) return error(null, data, "no_decoders");
        for (var i=0; i<decs.length; i++) {
            var d = decs[i];
            var spec = d.spec;
            const {err, decoded} = decode(spec, data, state.model)
            if (!err && decoded) return {decoded: {msg: d.msg, data: decoded}};
        }

        return error(null, data, "all_decoders_failed");
    }




    function assetUrl(baseUrl, name) {
        return baseUrl + 'js/' + name + '.js';
    }
    
    function encodeCmds(spec, data) {
        switch(typeof(spec)) {
            case "object":
                if (Array.isArray(spec)) {
                    var encoded = [];
                    for (var i=0; i<spec.length; i++) {
                        var cmd = { effect: spec[i].effect };
                        if (spec[i].hasOwnProperty("encoder")) {
                            var {err, value} = encode(spec[i].encoder, data, {});
                            if (err) return error(spec, data, "unsupported_encoder");
                            cmd.encoder = value;
                        } 
                        encoded[i] = cmd;
                    }
                    return {value: encoded};
                } else {
                    return encode(spec, data, {})
                }
            default:
                return error(spec, data, "unsupported_cmds")
        }
    }

    function applyCmds(encoders, effects, cmds, m2) {
        var {err, value} = encodeCmds(cmds, m2);
        if (err) return error(cmds, m2, err);
        value.forEach((cmd) => {
            const { effect, encoder } = cmd;
            const eff = effects[effect];
            if (!eff) {
                console.error("No such effect", cmd);
                return;
            }

            var enc = null;
            if (encoder) {
                enc = encoders[encoder];
                if (!enc) {
                    console.error("No such encoder", cmd);
                    return;
                }
            }

            setTimeout(() => {   
                eff(encoders, enc, Object.assign({}, m2));
            }, 0);
        });
    }

    function updateModel(spec, data) {
        if (!spec) return;
        var {err, value} = encode(spec, data, state.model);
        if (err) return err;
        log("[core] merging into model", value);
        Object.assign(state.model, value);
        log("[core] new model", state.model);
    }

    function withWhere(spec, data) {
        if (!spec.where) return {value: data};
        var {err, value} = encode(spec.where, data);
        if (err) return error(spec.where, data, err);
        return {value: Object.assign(data, value)};
    }

    function log(msg, data) {
        if (state.app.settings.debug) console.log(msg, data);
    }
    
    function selectUpdate(msg, update, ctx) {
        var clauses = update[msg];
        if (!clauses || !clauses.length) return error(msg, ctx, "no_update_implemented");
        for (var i=0; i<clauses.length; i++) {
            var c = clauses[i];
            if (!c.condition) return {spec: c};
            const { err, value } = encode(c.condition, ctx);
            log("[core] condition", { 
                condition: c.condition,
                context: ctx, 
                error: err, 
                value: value
            });
            if (err) return error(c.condition, model, err);
            if (value) return {spec: c};
        }
        return error(clauses, ctx, "all_conditions_failed");
    }

    function _update(ev) {
        const { encoders, effects, decoders, update } = state.app;
        const t0 = new Date();
        var {err, decoded } = tryDecoders(ev, decoders);
        if (err) {
            console.error("Decode error", err);
            return;
        }
        const { msg, data } = decoded;
        log("[core] decoded", decoded);
        const t1= new Date();
        var ctx = Object.assign({}, state.model, data);
        var {err, spec} = selectUpdate(msg, update, ctx);
        if (err) {
            console.error("Update error", err);
            return;
        } 
        var {err, value} = withWhere(spec, ctx);
        if (err) {
            console.error("Where error", err);
            return;
        }
        ctx = value;
        log("[core] update spec", spec, ctx);
        var err = updateModel(spec.model, ctx);
        if (err) {
            console.error("Update error", err);
            return;
        }
        const t2 = new Date();
        applyCmds(encoders, state.effects, spec.cmds, state.model);
        const t3 = new Date();
        if (state.app.settings.telemetry) {
            console.log("[core]"
                + "[decode " + elapsed(t0, t1) + "ms]"
                + "[update " + elapsed(t1, t2) + "ms]"
                + "[cmds " + elapsed(t2, t3) + "ms]");
        }
    }

    function update(ev) {
        setTimeout(function() {
            _update(ev);
        }, 0)
    }
    
    function init(app) {
        const { init } = app;
        const { model, cmds } = init;

        const t0 = new Date();
        const { err, value } = encode(model, {});
        if (err) {
            console.error("(init) cannot encode model", err);
        } else {
            state.model = value;
            const t1 = new Date();
            applyCmds(app.encoders, state.effects, cmds, state.model);
            const t2 = new Date();
            if (state.app.settings.telemetry) {
                console.log("[core]"
                    + "[init " + elapsed(t0, t1) + "ms]"
                    + "[cmds " + elapsed(t1, t2) + "ms]");
            }
        }
    }


    function indexedDecoders(effects, index) {
        var index = {};
        for (var k in effects) {
            if (effects.hasOwnProperty(k)) {
                var decs = effects[k];
                for (var dec in decs) {
                    if (decs.hasOwnProperty(dec)) {
                        var d = decs[dec];
                        var effSpec = d.object && d.object.effect ? d.object.effect : k;
                        var {err, value} = encode(effSpec, {});
                        if (err) return {err: err};
                        var eff = value;
                        if (!index[eff]) index[eff]=[];
                        index[eff].push({ msg: dec, spec: d});
                    }
                }
            }
        }
        return {value: index};
    }

    function compiledApp(app) {
        var {err, value} = indexedDecoders(app.decoders);
        if (err) {
            console.error("(decoders) failed to index decoders", err);
            return;
        }
        app.decoders = value;
        app.settings = app.settings || {};
        return app;
    }
    
    function effects(mods, next) {
        const out = {};
        for (var n in mods) {
            if (mods.hasOwnProperty(n)){
                const effSettings = state.app.effects[n] ?
                    (state.app.effects[n].settings || {}) : {};
                const settings = Object.assign({}, state.app.settings, effSettings);
                const mod = mods[n];
                const send = mod(n, settings, {
                    encode,
                    decode,
                    update,
                    tc
                });
                if (send instanceof Function) {
                    out[n] = send;
                } else {
                    console.warn('effect ' + n + ' is not returning a send function', send);
                }
            }
        }
        next(out);
    }

    function app(url, next) {
        fetch(url)
            .then((mod) => {
                return mod.json();
            })
            .then(next)
            .catch((err) => {
                console.error("app error", err);
            });
    };
    
    app(appUrl, function(app) {
        state.app = compiledApp(app);
        if (state.app.settings.debug) console.log(app);
        effects(appEffects, (effs) => {
            state.effects = effs;
            init(app);
        });
    });

};
