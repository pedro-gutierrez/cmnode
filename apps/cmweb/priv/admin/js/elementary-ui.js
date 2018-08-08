export default (name, settings, app) => {
    const { encode, decode, update, tc} = app;
    const state = {};
  
    function log(msg, data) {
        if (settings.debug) console.log(msg, data);
    }

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
        var items = value;
        if (!items || !items.length) return {view: []};
        var { err, value } = encode(spec.context, ctx);
        if (err) return error(spec, ctx, err);
        var sharedCtx = value;
        var out = [];
        for (var i=0; i<items.length; i++) {
            var item = items[i];
            var itemCtx = Object.assign(withSettings({item}), {context: sharedCtx});
            itemCtx.index = i;
            var { err, view } = compile(views, itemView, itemCtx);
            if (err) return error(spec, ctx, err);
            out.push(view);
        }
        return {view: out};
    }

    function compileViewRef(views, spec, ctx) {
        var { err, view } = resolve(views, spec.name, ctx);
        if (err) return error(spec, ctx, err);
        var {err, value} = encode(spec.params, ctx);
        if (err) return error(spec, ctx, err);
        var params = value;
        var {err, value} = encode(spec.condition, ctx);
        if (err) return error(spec, ctx, err);
        return value ? compile(views, view, withSettings(params)) 
            : {view: ['div']};
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
    
    mapboxgl.accessToken = "pk.eyJ1IjoiY29kZW11dGlueSIsImEiOiJjamk4b3RrZHAwbHVhM3BtNWx1eDg3eXFnIn0.jXq3glh_ARDIsVKUUo9jsw";

    function compileMapbox(views, spec, ctx) {
        var {err, value} = encode(spec.map.id, ctx);
        if (err) return error(spec, ctx, err);
        var id = value;
        var {err, value} = encode(spec.map.center, ctx);
        if (err) return error(spec, ctx, err);
        var center = value;
        var {error, value} = encode(spec.map.markers, ctx);
        if (err) return error(spec, ctx, err);
        var markers = value;
        
        setTimeout(() => {
            var map = new mapboxgl.Map({
                container: id,
                style: "mapbox://styles/mapbox/" + spec.map.style + "-v9",
                zoom: spec.map.zoom,
                center: [center.lon, center.lat]
            });
            
            markers.forEach((marker) => {
                var m = new mapboxgl.Marker();
                m.setLngLat([marker.lon, marker.lat]);
                m.addTo(map);
            });
        }, 0);
        return { view: ['div', { 
            style: "width: 100%; height: 300px",
            id: id
        }]};
    }

    function compileTimestamp(view, spec, ctx) {
        var {err, value} = encode(spec, ctx);
        if (err) return error(spec, ctx, err);
        return { view: value };
    }


    function compileJsonTerm(value) {
        if (!value) return [ 'span', { class: 'undefined'}, "Undefined" ];
        var kind = typeof(value);
        switch (kind) {
            case 'object':
                if (Array.isArray(value)) return [ 'ul', 
                    { class: 'list'}
                ].concat(value.map((v) => {
                    return ['li', { class: 'item' }, compileJsonTerm(v) ];
                }));

                return [ 'ul',
                    { class: 'object'}
                ].concat(Object.keys(value).filter((k) => {
                    return value.hasOwnProperty(k);
                }).map((k) => {
                    return ['li', { class: 'prop' }, 
                        [ 'strong', { class: 'name' }, compileJsonTerm(k), ['span', { class: 'separator' }, ':' ]],
                        compileJsonTerm(value[k]) ];
                }));
            default:
                return [ 'span', { class: "value " + kind  }, "" + value ];
        }
    }

    function compileJson(views, spec, ctx) {
        var {err, value} = encode(spec.json.source, ctx);
        if (err) return error(spec, ctx, err);
        return { view: ['div', {
            class: "json",
        }, compileJsonTerm(value)]};
    }

    function compile(views, spec, ctx) {
        if (Array.isArray(spec)) return compileList(views, spec, ctx);       
        if (spec.name) return compileViewRef(views, spec, ctx);
        if (spec.tag) return compileTag(views, spec, ctx);
        if (spec.text) return compileText(views, spec, ctx);
        if (spec.loop) return compileLoop(views, spec, ctx);
        if (spec.either) return compileEither(views, spec, ctx);
        if (spec.map) return compileMapbox(views, spec, ctx);
        if (spec.timestamp) return compileTimestamp(views, spec, ctx);
        if (spec.json) return compileJson(views, spec, ctx);
        return error(spec, ctx, "view_not_supported");
    }

    function render(view) {
        IncrementalDOM.patch(document.body, jsonml2idom, view);
    }

    function withSettings(ctx) {
        return Object.assign({}, settings, ctx);
    }
    
    function withContext(spec, ctx) {
        return Object.assign(ctx,{ context: spec.context });
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
        if (settings.telemetry) {
            console.log("[" + name + "]"
                + "[compile " + c.millis + "ms]"
                + "[render " + r.millis +"ms]");
        }
    }
};
