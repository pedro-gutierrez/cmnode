-module(cmxml).
-export([
         parse/3
        ]).

parse(File, Fun, State) ->
    xmerl_sax_parser:file(File, [{event_fun, Fun},
                                 {encoding, utf8},
                                    {event_state, State}]).
