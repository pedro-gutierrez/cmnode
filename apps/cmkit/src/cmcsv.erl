-module(cmcsv).
-export([fold/3, parse/3, now/0]).

fold(File, Acc, Fun) ->
    case file:open(File, [raw, read_ahead, binary]) of
        {ok, IoDevice} ->
            case without_header(IoDevice) of
                {ok, _} ->
                    Acc2 = fold_line(IoDevice, Acc, Fun),
                    file:close(IoDevice),
                    Acc2;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end.

fold_line(IoDevice, Acc, Fun) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            Unicode = uniconvert(binary_to_list(Data)), 
            Data2 = binary:part(Unicode, {0, size(Unicode)-1}),
            Fields = binary:split(Data2, <<",">>, [global]),
            Acc2 = Fun(Fields, Acc),
            fold_line(IoDevice, Acc2, Fun);
        eof ->
            Acc
    end.

parse(File, BatchSize, Fun) ->
    Stats = #{ start => cmcsv:now(), 
               read => 0,
               stop => undef,
               written => 0 
             },
    case file:open(File, [raw, read_ahead, binary]) of
        {ok, IoDevice} ->
            case without_header(IoDevice) of
                {ok, _} ->
                    R = read_batch(IoDevice, BatchSize, BatchSize, Stats, [], Fun),
                    file:close(IoDevice),
                    R;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end.

read_batch(IoDevice, 0, BatchSize, #{ written := Written}=Stats, Batch, Fun) ->
    case Fun(Batch) of 
        {ok, Processed} -> 
            read_batch(IoDevice, BatchSize, BatchSize, Stats#{ written => Written + Processed }, [], Fun);
        Error ->
            file:close(IoDevice),
            Error
    end;

read_batch(IoDevice, Current, BatchSize, #{ start := Start,
                                            written := Written, 
                                            read := LinesRead }=Stats, Batch, Fun) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            Unicode = uniconvert(binary_to_list(Data)), 
            Data2 = binary:part(Unicode, {0, size(Unicode)-1}),
            Fields = binary:split(Data2, <<",">>, [global]),
            read_batch(IoDevice, Current-1, BatchSize, Stats#{ read => LinesRead + 1}, [Fields|Batch], Fun);
        eof ->
            case Fun(Batch) of 
                {ok, Processed} -> 
                    Stop = cmcsv:now(),
                    LinesRead2 = LinesRead + 1,
                    Written2 = Written + Processed,
                    Elapsed = (Stop - Start) / 1000,
                    ReadRate = LinesRead2 / Elapsed,
                    WriteRate = Written2 / Elapsed,
                    {ok, Stats#{ read => LinesRead2, 
                                 written => Written2,
                                 elapsed => #{ secs => Elapsed, 
                                               millis => Stop - Start},
                                 rate => #{ read => ReadRate,
                                            write => WriteRate
                                          },

                                 stop => Stop }};
                Error -> 
                    {error, Error}
            end;
        Other -> 
            Other
    end.

without_header(IoDevice) ->
    file:read_line(IoDevice).

now() ->
    erlang:system_time(millisecond).

uniconvert(String) ->
    try xmerl_ucs:from_utf8(String) of
        _ ->
            list_to_binary(String)
    catch
        exit:{ucs,{bad_utf8_character_code}} ->
            list_to_binary(xmerl_ucs:to_utf8(String))
    end.
