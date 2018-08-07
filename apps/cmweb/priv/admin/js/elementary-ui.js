export default (name, settings, app) => {
    const { encode, decode, update, tc} = app;
    const state = {};
   
    function event(eventName, value) {
        const e = { effect: name, event: eventName, value: value };
        update(e);
    }

    function error(spec, data, reason) {
        return {err: {spec, data, reason }};
    }

    function evValue(target) {
        return target.files || target.value || "";
    }

    function addHandlers(attrs) {
        function _(keys, out) {
            if (!keys.length) return out;
            var k = keys.shift();
            var v = attrs[k];
            switch (k) {
                case "onclick":
                    out[k] = () => { event(v); }
                    break;
                case "onchange":
                    out.oninput = (ev) => {
                        event(v, evValue(ev.target))
                    }
                    break;
                case "oninput":
                    out.onchange = (ev) => {
                        event(v, evValue(ev.target))
                    }
                    break;
                default: 
                    out[k] = v
            }
            return _(keys, out);
        };
        return _(Object.keys(attrs), attrs);
    }
    
    function resolve(views, spec, ctx) {
        switch(typeof(spec)) {
            case 'object':
                const { err, value } = encode(spec, ctx);
                if (err) return error(spec, ctx, "view_name_encode_error");
                return resolve(views, value, ctx);
            case 'string':
                const resolved = views[spec];
                return resolved ? { view: resolved.view } 
                    : error(spec, ctx, "no_such_view");
            default:
                return error(spec, ctx, "unsupported_view_name_spec");
        }
    }
    
    function compileList(views, items, ctx) {
        if (!items) return { view: [] };
        function _(rem, out) {
            if (!rem.length) return { view: out };
            var item = rem.shift();
            const {err, view } = compile(views, item, ctx);
            if (err) return {err};
            out.push(view);
            return _(rem, out);
        }
        return _(items.slice(0), []);
    }
    
    function compileLoop(views, spec, ctx) {
        var { err, view } = resolve(views, spec.with, ctx);
        if (err) return error(spec, ctx, err);
        var itemView = view;
        var { err, value } = encode(spec.loop, ctx);
        if (err) return error(spec, ctx, err);
        function _(i, out) {
            if (i == value.length) return {view: out};
            var item = value[i];
            var { err, view } = compile(views, itemView, withSettings({item}));
            if (err) return {err};
            out.push(view);
            return _(i+1,out);
        }
        return _(0, []);
    }

    function compileViewRef(views, spec, ctx) {
        var { err, view } = resolve(views, spec.name, ctx);
        if (err) return error(spec, ctx, err);
        var {err, value} = encode(spec.params, ctx);
        if (err) return error(spec, ctx, err);
        return compile(views, view, withSettings(value));
    }

    function compileTag(views, spec, ctx) {
        const { tag, attrs, children } = spec;
        var { err, value } = encode(attrs, ctx);
        if (err) return error(spec, ctx, err);
        var { err, view } = compile(views, children, ctx);
        if (err) return {err};
        return { view: [tag, addHandlers(value) ].concat(view) };
    }

    function compileText(views, spec, ctx) {
        const {err, value} = encode(spec.text, ctx);
        if (err) return error(spec, ctx, err);
        return { view: value }
    }

    function compile(views, spec, ctx) {
        if (Array.isArray(spec)) return compileList(views, spec, ctx);       
        if (spec.name) return compileViewRef(views, spec, ctx);
        if (spec.tag) return compileTag(views, spec, ctx);
        if (spec.text) return compileText(views, spec, ctx);
        if (spec.loop) return compileLoop(views, spec, ctx);
        if (spec.either) return compileEither(views, spec, ctx);
        return error(spec, ctx, "not_supported");
    }

    function render(view) {
        IncrementalDOM.patch(document.body, jsonml2idom, view);
    }

    function withSettings(ctx) {
        return Object.assign({}, settings, ctx);
    }
    
    return (views, v, model) => {
        if (v) state.view = v;
        
        var c = tc(() => { 
            return compile(views, state.view.view, withSettings(model));
        });
        if (c.res.err) {
            console.error("Can't compile view", c.res.err);
            return;
        } 
        
        var r = tc(() => { render(c.res.view) });
        console.log("[compile " + c.millis + "ms] [render " + r.millis +"ms]");
    }
};
