-module(cmcalendar).
-export([
         last/1, 
         to_bin/2, 
         add_seconds_to_utc/1,
         add_seconds_to_utc/2]).

add_seconds_to_utc(Secs) -> 
    add_seconds_to_utc(Secs, calendar:universal_time()).

add_seconds_to_utc(Secs, DateTime) -> 
    DateTime2 = iso8601:add_time(DateTime, 0, 0, Secs),
    cmkit:date_as_map(DateTime2).

last({Num, days}) ->
    Secs = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    back({Num-1, days}, Secs, []).

back({Days, days}, From) ->
    calendar:gregorian_seconds_to_datetime(From - Days * 24 * 3600).

back({0, _}, From, Out) -> [calendar:gregorian_seconds_to_datetime(From)|Out];
back({N, days}, From, Out) ->
    back({N-1, days}, From, [back({N, days}, From)|Out]).

to_bin({{Y, _, _}, _}, year) -> 
    cmkit:to_bin(Y);

to_bin({{Y, M, _}, _}, month) -> 
    cmkit:bin_join([ cmkit:to_bin(Y), 
                     cmkit:to_bin(M) 
                   ], <<"-">>);

to_bin({{Y, M, D}, _}, date) -> 
    cmkit:bin_join([ cmkit:to_bin(Y), 
                     cmkit:to_bin(M),
                     cmkit:to_bin(D)
                   ], <<"-">>).


