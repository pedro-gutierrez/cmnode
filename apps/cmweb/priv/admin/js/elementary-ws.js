export default (name, settings, app) => {
    const { encode, decode, update} = app;
    const state = {};

    function url() {
        if (settings.url) return settings.url;
        const scheme = window.location.protocol === 'http:' ? 'ws' : 'wss';
        return scheme + '://' + window.location.host + (settings.path||'/ws');
    }

    function send(ev) {
        update(ev);
    }

    state.url = url();

    function keepAlive() {
        state.ws.send("");
        setTimeout(keepAlive, 15*1000);
    }

    function reconnect() {
        console.warn('Reconnecting in 5secs...');
        setTimeout(connect, 5*1000);
    }

    function connect() {
        state.ws = new WebSocket(state.url);
        state.ws.onopen = () => {
            if (settings.persistent) keepAlive();
            send({ effect: name, event: 'connected' })};
        state.ws.onclose = () => {
            if (settings.persistent) reconnect();
            send({ effect: name, event: 'disconnected' })};
        state.ws.onerror = () => {
            send({ effect: name, event: 'error' })};
        state.ws.onmessage = (ev) => {
            send({ effect: name, event: 'data', data: JSON.parse(ev.data) })};
    }

    connect();

    return (encoders, enc, model) => {
        const {err, value} = encode(enc, model);
        if (err) {
            console.error("Encode error", err);
        } else state.ws.send(JSON.stringify(value));
    }
};
