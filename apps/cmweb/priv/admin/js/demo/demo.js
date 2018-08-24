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
                domEl: "demoApp"
            }
        }
    },
    encoders: {
        demoView: {
            view: {
                tag: "div",
                children: [
                    { 
                        view: "introView" 
                    },
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
                tag: "ul",
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
                tag: "li",
                attrs: {
                    object: {
                        key: {
                            key: "title",
                        },
                        class: {
                            key: "state",
                        }
                    }
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
                    },
                    { 
                        view: "label",
                        params: {
                            title: {
                                key: "title",
                            }
                        }
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
                    { 
                        text: {
                            key: "title"
                        }
                    }
                ]
            }
        },
        introView: {
            view: {
                tag: "article",
                children: [
                    {
                        tag: "h1",
                        children: [
                            { 
                                text: "A simple To-Do App" 
                            }
                        ]
                    },
                    {
                        tag: "p",
                        children: [
                            {
                                text: "In this simple demo, you can create tasks, mark them as done/undone, and delete them." }
                        ]
                    },
                    {   tag: "p",
                        children: [
                            { 
                                text: "Feel free to download the complete " 
                            },
                            { 
                                view: "link",
                                params: {
                                    title: "source",   
                                    target: "/js/demo/demo.js"
                                }
                            },
                            { 
                                text: " of this app. Also, make sure you don't forget to read the " 
                            },
                            { 
                                view: "link",
                                params: {
                                    title: "guide",   
                                    target: "/guide.html"
                                }
                            },
                            { 
                                text: "." 
                            }
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
