-module(cmcron).
-export([reload/0, status/0]).

reload() -> call(reload).

status() -> call(status).

call(Msg) ->
    gen_server:call({cmcron_server, node()}, Msg).

