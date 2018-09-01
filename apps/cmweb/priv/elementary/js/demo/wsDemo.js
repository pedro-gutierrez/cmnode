//
// ElementaryJS.com
// (c) 2018 Pedro Gutierrez
// Released under the MIT License.
// 
import {default as app} from "/js/elementary.js";
import {default as ui} from "/js/elementary-ui.js";
import {default as ws} from "/js/elementary-ws.js";

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
        domEl: "wsApp"
      }
    },
    ws: {
      fun: ws,
      settings: {
        debug: true,
        domEl: "wsApp",
        path: "/echo"
      }
    },
  },
  init: {
    model: {
      status: "offline",
      myMessage: "",
      serverMessage: "",
    },
    cmds: [
      { effect: "ui", encoder: "demoView" }
    ]
  },
  decoders: {
    connected: {
      effect: "ws",
      event: "connected"
    },
    disconnected: {
      effect: "ws",
      event: "disconnected"
    },
    messageChanged: {
      effect: "ui",
      event: "messageChanged",
      value: { 
        any: "text" 
      }
    },
    messageSubmitted: {
      effect: "ui",
      event: "messageSubmitted",
      value: { 
        any: "text" 
      }
    },
    messageReceived: {
      effect: "ws",
      event: "data",
      data: {
        reply: {
          any: "text"
        }
      }
    }
  },
  update: {
    connected: {
      model: {
        status: "online"
      },
      cmds: [
        { effect: "ui" }
      ]
    },
    disconnected: {
      model: {
        status: "offline"
      },
      cmds: [
        { effect: "ui" }
      ]
    },
    messageChanged: {
      model: {
        myMessage: {
          key: "value"
        }
      },
      cmds: [
        { effect: "ui" }
      ]
    },
    messageSubmitted: {
      model: {
        myMessage: {
          key: "value"
        }
      },
      cmds: [
        { effect: "ui" },
        { effect: "ws", encoder: "newMessage" }
      ]
    },
    messageReceived: {
      model: {
        myMessage: "",
        serverMessage: {
          key: "reply",
          in: "data"
        }
      },
      cmds: [
        { effect: "ui" }
      ]
    } 
  },
  encoders: {
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
                  { text: "Websockets" }
                ]
              },
              {
                tag: "p",
                children: [
                  { 
                    text: "A simple echo application that shows how to connect, send and receive data over a websocket connection." 
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
                      href: "/js/demo/wsDemo.js",
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
                view: "messageView",
                when: {
                  and: [
                    { 
                      isSet: "serverMessage"
                    },
                    { 
                      equal: [
                        { key: "status" },
                        { text: "online" }
                      ]
                    }
                  ]
                },
                params: {
                  title: "Server echoed:",
                  message: {
                    key: "serverMessage"
                  }
                }
              },

              { 
                view: "inputView",
                when: {
                  equal: [
                    { key: "status" },
                    { text: "online" }
                  ]
                },
                params: {
                  message: { key: "myMessage" }
                }
              },
              { 
                view: {
                  format: {
                    pattern: "~aIndicatorView",
                    params: [
                      { key: "status" }
                    ]
                  }
                }
              }
            ]
          }
        ]
      }
    },
    messageView: {
      view: {
        tag: "div",
        attrs: {
          class: "field"
        },
        children: [
          { 
            tag: "label", 
            attrs: {
              class: "label"
            },
            children: [
              { 
                text: { key: "title" } 
              }
            ]
          },
          {
            tag: "p",
            children: [
              { 
                text: { key: "message" } 
              }
            ]
          }
        ]
      }
    },
    inputView: {
      view: {
        tag: "div",
        attrs: {
          class: "field"
        },
        children: [
          { 
            tag: "div",
            attrs: { 
              class: "control" 
            },
            children: [
              { 
                tag: "input", 
                attrs: { 
                  class: "input", 
                  type: "text", 
                  placeholder: "Say something and press Enter" ,
                  onkeyup: {
                    expression: {
                      either: [
                        {
                          when: {
                            equal: [
                              { text: "Enter" },
                              { key: "key", in: "event" }
                            ]
                          },

                          then: "messageSubmitted"
                        },
                        { then: "messageChanged" }
                      ]
                    }
                  },
                  value: { key: "message" }
                }
              }
            ]
          }
        ]
      }
    },
    onlineIndicatorView: {
      view: {
        tag: "span",
        attrs: {
          class: "tag is-success"
        },
        children: [
          { text: "Connected" }
        ]
      }
    },
    offlineIndicatorView: {
      view: {
        tag: "span",
        attrs: {
          class: "tag is-danger"
        },
        children: [
          { text: "Disconnected" }
        ]
      }
    },
    newMessage: {
      message: {
        key: "myMessage"
      }
    }
  }
});
