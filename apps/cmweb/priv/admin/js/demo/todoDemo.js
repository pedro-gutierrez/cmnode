/*!
 * ElementaryJS.com
 * (c) 2018 Pedro Gutierrez
 * Released under the MIT License.
 */
import {default as app} from "/js/demo/elementary.js";
import {default as ui} from "/js/demo/elementary-ui.js";

app({
    settings: {
        debug: true,
        telemetry: true
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
    effects: {
        ui: {
            fun: ui,
            settings: {
                debug: false,
                domEl: "todoDemoApp"
            }
        }
    },
    encoders: {
        demoView: {
            view: {
                tag: "div",
                attrs: {
                    class: "columns"
                },
                children: [
                    { view: "introView" },
                    { view: "appView",
                      params: {
                        tasks: { key: "tasks" },
                        taskTitle: { key: "taskTitle" }  
                      }
                    }
                ]
            }
        },
        appView: {
            view: {
                tag: "div",
                attrs: {
                    class: "column col-6 col-xl-6 col-lg-12 col-md-12 col-sm-12 col-xs-12 app"
                },
                children: [
                    { 
                        view: "tasksView",
                        params: {
                            tasks: {
                                key: "tasks"
                            }
                        }
                    },
                    {
                        view: "taskForm",
                        params: {
                            title: {
                                key: "taskTitle"
                            }
                        }
                    }
                ]
            }
        },
        taskForm: {
            view: {
                tag: "input",
                attrs: {
                    class: "mt-2",
                    type: "text",
                    value: {
                        key: "title"
                    },
                    autocomplete: "off",
                    placeholder: "Enter something to do and press Enter",
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

                                { 
                                    then: "taskTitleChanged" 
                                }
                            ]
                        }
                    }
                }
            }
        },
        tasksView: {
            view: {
                tag: "div",
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
                                pattern: "columns mt-2 ~a",
                                params: [
                                    { key: "state" }
                                ]
                            }
                        }
                    }
                },
                children: [
                    { 
                        tag: "div",
                        attrs: { 
                            class: "column col-6"
                        },
                        children: [
                            { 
                                view: "label",
                                params: {
                                    title: {
                                        key: "title",
                                    }
                                }
                            }
                        ]
                    },
                    { 
                        tag: "div",
                        attrs: { 
                            class: "column text-right col-6"
                        },
                        children: [
                            {
                                view: "button",
                                params: {
                                    style: "primary",
                                    whenPressed: {
                                        name: "changeTaskState",
                                        task: {
                                            key: "title",
                                        }
                                    },
                                    title: {
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
                            },
                            {
                                view: "button",
                                condition: {
                                    equal: [
                                        { 
                                            key: "state",
                                        },
                                        { 
                                            text: "done" 
                                        }
                                    ]
                                },
                                params: {
                                    style: "danger",
                                    whenPressed: {
                                        name: "deleteTask",
                                        task: {
                                            key: "title",
                                        }
                                    },
                                    title: "Delete"
                                }
                            }
                        ]
                    }
                ]
            }
        },
        label: {
            view: {
                tag: "label",
                children: [
                    { 
                        text: {
                            key: "title"
                        }
                    }
                ]
            }   
        },
        button: {
            view: {
                tag: "button",
                attrs: {
                    class: {
                        format: {
                            pattern: "btn btn-sm ~a",
                            params: [
                                { key: "style" }
                            ]
                        }
                    },
                    onclick: { 
                        key: "whenPressed"
                    }
                },
                children: [
                    {
                        text: {
                            key: "title"
                        }
                    }
                ]
                
            }
        },
        link: {
            view: {
                tag: "a",
                attrs: {
                    href: {
                        key: "target"
                    }
                },
                children: [
                    { tag: "i",
                      attrs: {
                        class: { key: "icon" }}},
                    { text: { key: "title" }}
                ]
            }
        },
        introView: {
            view: {
                tag: "article",
                attrs: {
                    class: "col-6 col-xl-6 col-lg-12 col-md-12 col-sm-12 col-xs-12"
                },
                children: [
                    {
                        tag: "h1",
                        children: [
                            { 
                                text: "To-Do App" 
                            }
                        ]
                    },
                    {
                        tag: "p",
                        children: [
                            {
                                text: "This simple app shows the basics of how to use the UI effect manager in order to render your model, and manage state transitions by responding to mouse and keyboard events." }
                        ]
                    },
                    {   tag: "p",
                        children: [
                            { 
                                view: "link",
                                params: {
                                    icon: "fas fa-download",
                                    title: "Source",   
                                    target: "/js/demo/todoDemo.js"
                                }
                            },
                        ]
                    }
                ]
            }
        }
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
