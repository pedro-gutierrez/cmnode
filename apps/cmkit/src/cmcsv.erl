-module(cmcsv).
-export([parse/3]).

parse(File, BatchSize, Fun) ->
    Stats = #{ start => cmkit:now(), 
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
            Unicode = cmkit:uniconvert(binary_to_list(Data)), 
            Data2 = binary:part(Unicode, {0, size(Unicode)-1}),
            Fields = binary:split(Data2, <<",">>, [global]),
            read_batch(IoDevice, Current-1, BatchSize, Stats#{ read => LinesRead + 1}, [Fields|Batch], Fun);
        eof ->
            case Fun(Batch) of 
                {ok, Processed} -> 
                    Stop = cmkit:now(),
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
                    {err, Error}
            end;
        Other -> 
            Other
    end.

without_header(IoDevice) ->
    file:read_line(IoDevice).
