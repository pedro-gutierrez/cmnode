export default (name, settings, app) => {
    const { encode, decode, update, tc} = app;
    
    function baseUrl() {
        if (settings.url) return settings.url;
        var {protocol, host} = window.location;
        return `${protocol}//${host}${settings.path}`
    }
    
    const state = { 
        url: baseUrl()
    };

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
            error("error decoding json", { text: raw, reason: e});
            return raw;
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

    const JSON_MIME = 'application/json'; 

    function decodeBody(headers, req) {
        var raw = req.responseText;
        return headers['content-type'] === JSON_MIME ?
            decodeJson(raw) : raw;
    }

    function encodeBody(req, next) {
        switch (req.headers["content-type"]) {
            case JSON_MIME:
                return next(JSON.stringify(req.body), JSON_MIME);
            default:
                var form = new FormData();
                for (var f in req.body) {
                    if (req.body.hasOwnProperty(f)) {
                        form.append(f, req.body[f])
                    }
                }
                return next(form, null);
        }
    }

    return (encoders, enc, model) => {
        var {err, value} = encode(enc, model);
        if (err) {
            error("error encoding request", err);
        } else {
            encodeBody(value, (body, ct) => {
                var url = state.url + (value.path || '');
                var xhr = new XMLHttpRequest();
                xhr.open((value.method||'get').toUpperCase(), url );
                if (ct) xhr.setRequestHeader("content-type", ct);
                if (value.headers) {
                    for (var h in value.headers) {
                        if (value.headers.hasOwnProperty(h)) {
                            xhr.setRequestHeader(h, value.headers[h]);
                        }
                    }
                }
                xhr.onload = function () {
                    const headers = getHeadersAsObject(xhr);
                    update({
                        effect: name,
                        status: xhr.status,
                        headers: headers,
                        body: decodeBody(headers, xhr) 
                    })
                }
                xhr.send(body);
            });
        }
    };
}
