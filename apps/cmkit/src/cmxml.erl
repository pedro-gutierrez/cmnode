-module(cmxml).
-export([
         parse/3
        ]).

parse(Bin, Fun, State) when is_binary(Bin) ->
    xmerl_sax_parser:stream(Bin, [{event_fun, Fun},
                                  {encoding, utf8},
                                  {event_state, State}]).
