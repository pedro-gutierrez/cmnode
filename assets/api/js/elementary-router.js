export default (name, settings, app) => {
    const {encode, decode, update, tc} = app;
    
    function error(spec, data, reason) {
        console.error(`[$name]`, {spec, data, reason});
    }

    function route() {
        const hashParts = window.location.hash.split('/');
        hashParts.shift();
        update({ 
            effect: name,
            route: hashParts
        });
    }

    return (encoders, req, model) => {
        const {err, value} = encode(req, model);
        if (err) return error(req, model, err);
        const {action, target} = value;
        switch(action) {
            case 'get':
                return route();
            case 'navigate':
                window.location.hash = '#/' + target;
                return route();
            default:
                console.warn("[elementary-router] not implemented", req);
                return;
        }
    };
}
