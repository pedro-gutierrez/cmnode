export default (name, settings, app) => {
  const { encode, decode, update} = app;
  const state = {};

  function error(msg, data){
    console.error(`[${name}] ${msg}`, data);  
  }
    
  function debug(msg, data) {
    if (settings.debug) console.log(`[${name}] ${msg}`, data);
  }

  function buildUrl() {
    if (settings.url) return {url: settings.url};
    if (!settings.path) return { err: "Missing path in settings" };
    const scheme = window.location.protocol === 'http:' ? 'ws' : 'wss';
    return {url: scheme + '://' + window.location.host + settings.path};
  }

  function send(ev) {
    update(ev);
  }


  function keepAlive() {
    state.ws.send("");
    setTimeout(keepAlive, 15*1000);
  }

  function reconnect(url) {
    console.warn(`[${name}] Reconnecting in 5secs...`);
    setTimeout(function() {
      connect(url) 
    }, 5*1000);
  }

  function connect(url) {
    state.ws = new WebSocket(url);
    state.ws.onopen = () => {
      if (settings.persistent) keepAlive();
      send({ effect: name, event: 'connected' })};
    state.ws.onclose = () => {
      if (settings.persistent) reconnect(url);
      send({ effect: name, event: 'disconnected' })};
    state.ws.onerror = () => {
      send({ effect: name, event: 'error' })};
    state.ws.onmessage = (ev) => {
      send({ effect: name, event: 'data', data: JSON.parse(ev.data) })};
  }

  const {err, url} = buildUrl();
  if (err) {
    error(err, settings)
  } else {
    connect(url);
  }

  return (encoders, enc, model) => {
    const {err, value} = encode(enc, model);
    if (err) {
      error("encode error", err);
    } else state.ws.send(JSON.stringify(value));
  }
};
