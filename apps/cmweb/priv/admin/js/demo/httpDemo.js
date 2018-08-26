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
        model: {},
        cmds: [
            { effect: "ui", encoder: "demoView"}
        ]
    },
    effects: {
        ui: {
            fun: ui,
            settings: {
                debug: false,
                domEl: "httpDemoApp"
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
                    { 
                        view: "introView" 
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
                                text: "HTTP App" 
                            }
                        ]
                    },
                    {
                        tag: "p",
                        children: [
                            {
                                text: "This is a simple app that shows how to use the HTTP effect manager. It fetches the source code of the To-do app from the server, and renders it with nice syntax highlighting." }
                        ]
                    },
                    {   tag: "p",
                        children: [
                            { 
                                view: "link",
                                params: {
                                    icon: "fas fa-download",
                                    title: "Source",   
                                    target: "/js/demo/httpDemo.js"
                                }
                            },
                        ]
                    }
                ]
            }
        }
    },
    decoders: {
    },
    update: {
    }
});
