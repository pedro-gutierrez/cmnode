//
// ElementaryJS.com
// (c) 2018 Pedro Gutierrez
// Released under the MIT License.
//
import {default as app} from "/js/elementary.js";
import {default as ui} from "/js/elementary-ui.js";
import {default as http} from "/js/elementary-http.js";

app({
  settings: {
    debug: true,
    telemetry: true
  },
  effects: {
    ui: {
      fun: ui,
      settings: {
        debug: true,
        domEl: "httpApp"
      }
    },
    http: {
      fun: http,
      settings: {
        debug: true,
      }
    }
  },
  init: {
    model: {
      source: ""
    },
    cmds: [
      { effect: "http", encoder: "sourceQuery" },
      { effect: "ui", encoder: "demoView" }
    ]
  },
  encoders: {
    sourceQuery: {
      method: "get",
      url: "/js/demo/todoDemo.js",
    },
    demoView: {
      view: {
        tag: "div",
        children: [
          { 
            tag: "div",
            attrs: {
              class: "notification"
            },
            children: [
              {
                tag: "p", 
                attrs: {
                  class: "title"  
                },
                children: [
                  { text: "Http" }
                ]
              },
              {
                tag: "p",
                children: [
                  { 
                    text: "This app shows how to fetch remote server data via HTTP. Here, we download the source code of the Todo app, then we render it with some nice syntax highlighting." 
                  }

                ]
              },
              {
                tag: "p",
                attrs: {
                  class: "actions"
                },
                children: [
                  {
                    tag: "a",
                    attrs: {
                      href: "/js/demo/httpDemo.js",
                      class: "is-fullwidth is-link"
                    },
                    children: [
                      {
                        tag: "span",
                        children: [
                          { text: "Source" }
                        ]
                      }
                    ]
                  }
                ]
              }
            ]
          },
          {
            tag: "div",
            attrs: {
              class: "tile-content"
            },
            children: [
              { 
                code: { 
                  source: {
                    key: "source" 
                  },
                  lang: "javascript"
                }

              }
            ]
          }
        ]
      }
    }
  },
  decoders: {
    sourceFetched: {
      effect: "http",
      status: 200,
      body: {
        any: "text"
      }
    }
  },
  update: {
    sourceFetched: {
      model: {
        source: {
          key: "body"
        }
      },
      cmds: [
        { effect: "ui" }
      ]
    }
  }
});
