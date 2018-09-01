//
// ElementaryJS.com
// (c) 2018 Pedro Gutierrez
// Released under the MIT License.
//
import {default as app} from "/js/elementary.js";
import {default as ui} from "/js/elementary-ui.js";

app({
  settings: {
    debug: true,
    telemetry: true
  },
  effects: {
    ui: {
      fun: ui,
      settings: {
        debug: false,
        domEl: "todoApp"
      }
    },
  },
  init: {
    model: {
      taskTitle: "",
      tasks: [
        {
          title: "Read the guide",
          state: "done"
        }, 
        {
          title: "Write a cool ElementaryJS app",
          state: "pending"
        }, 
      ]
    },
    cmds: [
      {
        effect: "ui",
        encoder: "demoView"
      }
    ]
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
                  { text: "Basic" }
                ]
              },

              {
                tag: "p",
                children: [
                  { 
                    text: "A simple Todo app that shows how to render a view and  keep it in sync with the application model." 
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
                      href: "/js/demo/todoDemo.js",
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
                view: "tasksView",
                params: {
                  tasks: { key: "tasks" },
                }
              },
              { 
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
                          placeholder: "Enter a task and press Enter" ,
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

                                  then: "taskSubmitted"
                                },
                                { then: "taskTitleChanged" }
                              ]
                            }
                          },
                          value: { key: "taskTitle" }
                        }
                      }
                    ]
                  }
                ]
              }
            ]
          }
        ]
      }
    },
    tasksView: {
      view: {
        tag: "div",
        attrs: {
          class: "field"
        },
        children: {
          loop: {
            key: "tasks"
          },
          with: "taskView"
        }
      }
    },
    taskView: {
      view: {
        tag: "div",
        attrs: {
          object: {
            key: {
              key: "title",
            },
            class: {
              format: {
                pattern: "field ~a",
                params: [
                  { key: "state" }
                ]
              }
            }
          }
        },
        children: [
          { 
            tag: "span",
            children: [
              { text: { key: "title" } }
            ]
          },
          { 
            tag: "div",
            children: [
              { 
                tag: "a",
                when: {
                  equal: [
                    { 
                      key: "state",
                    },
                    { 
                      text: "done" 
                    }
                  ]
                },
                attrs: {
                  class: "is-link",
                  href: "#",
                  onclick: {
                    name: "deleteTask",
                    task: {
                      key: "title",
                    }
                  },
                },
                children: [
                  { text: "Delete " }
                ]
              },
              { 
                tag: "a",
                attrs: {
                  class: "is-link",
                  onclick: {
                    name: "changeTaskState",
                    task: {
                      key: "title",
                    }
                  },
                },
                children: [
                  { 
                    text: {
                      either: [
                        {
                          when: {
                            equal: [
                              { key: "state" },
                              { text: "pending" }
                            ]
                          },
                          then: {
                            text: "Done"
                          }
                        },
                        { 
                          then: {
                            text: "Undo" 
                          }
                        }
                      ]
                    }
                  }
                ]
              }
            ]
          },
        ]
      }
    },
  },
  decoders: {
    taskTitleChanged: {
      effect: "ui",
      event: "taskTitleChanged",
      value: {
        any: "text"
      }
    },
    taskSubmitted: {
      effect: "ui",
      event: "taskSubmitted"
    },
    changeTaskState: {
      effect: "ui",
      event: {
        name: "changeTaskState",
        task: {
          any: "text"
        }
      }
    },
    deleteTask: {
      effect: "ui",
      event: {
        name: "deleteTask",
        task: {
          any: "text"
        }
      }
    },

  },
  update: {
    taskTitleChanged: {
      model: {
        taskTitle: {
          key: "value"
        }
      },
      cmds: [
        { effect: "ui" }
      ]
    },
    taskSubmitted: {
      model: {
        taskTitle: "",
        tasks: {
          byAppending: {
            state: "pending",
            title: {
              key: "taskTitle"
            }
          }
        }
      },
      cmds: [
        { effect: "ui" }
      ]
    },
    changeTaskState: {
      model: {
        tasks: {
          byReplacing: {
            items: {
              title: {
                key: "task",
                in: "event"
              }
            },
            with: {
              byReplacing: {
                state: {
                  either: [
                    {
                      when: {
                        equal: [
                          { key: "state" },
                          { text: "pending" }
                        ]
                      },
                      then: { text: "done" }
                    },
                    { 
                      then: { text: "pending" } 
                    }
                  ]
                }
              }
            }
          }
        }
      },
      cmds: [
        { effect: "ui" }
      ]
    },
    deleteTask: {
      model: {
        tasks: {
          byRemoving: {
            title: {
              key: "task",
              in: "event"
            }
          }
        }
      },
      cmds: [
        { effect: "ui"}
      ]
    }
  }
});
