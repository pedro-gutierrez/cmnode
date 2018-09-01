export default (name, settings, app) => {
  const { encode, decode, update, tc} = app;
  const state = {};
  
  function error(msg, data){
    console.error(`[${name}] ${msg}`, data);  
  }

  function debug(msg, data) {
    if (settings.debug) console.log(`[http][${name}] ${msg}`, data);
  }

  function decodeJson(raw) {
    try {
      return JSON.parse(raw);
    } catch (e) {
      return null;
    }
  }

  function getHeadersAsObject(xhr){
    let headers = {}
    xhr.getAllResponseHeaders()
      .split('\u000d\u000a')
      .forEach((line) => {
        if (line.length > 0)
        {
          let delimiter = '\u003a\u0020',
            header = line.split(delimiter)

          headers[header.shift().toLowerCase()] = header.join(delimiter)
        }
      })
    return headers
  }

  function jsonOrRaw(headers, req) {
    return headers['content-type'] === 'application/json' ?
      (decodeJson(req.responseText) || req.responseText) : req.responseText;
  }

  return (encoders, enc, model) => {
    var {err, value} = encode(enc, model);
    if (err) {
      error("error encoding request", err);
    } else {

      const req = new XMLHttpRequest();
      req.open((value.method||'get').toUpperCase(), value.url||'/' );
      req.send();
      req.onload = function () {
        const headers = getHeadersAsObject(req);
        update({
          effect: name,
          status: req.status,
          headers: headers,
          body: jsonOrRaw(headers, req) 
        })
      }
    }
  };
}
