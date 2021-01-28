-module(cmdb_util).
-export([
         reload/0,
         reader/1,
         writer/1,
         write/2,
         read/1,
         read/2,
         read_all/2,
         read/3,
         fold/3,
         unwind/2,
         all_new/2 ]).

reload() ->
    Buckets = [ compiled(Spec) || {ok, Spec} <- cmkit:yamls(bucket), has_db_role(Spec) ] ,
    {ok, Buckets}.

has_db_role(#{ <<"spec">> := #{ <<"role">> := <<"es">> }}) -> false;
has_db_role(_) -> true.

compiled(#{ <<"name">> := Name,
            <<"spec">> := Spec}) ->

    #{ name => cmkit:to_atom(Name),
       debug => cmkit:to_atom(maps:get(<<"debug">>, Spec, <<"false">>))}.

reader(N) -> {bucket, reader, N}.
writer(N) -> {bucket, writer, N}.

write(Name, Msg) ->
    case cmbus:members(writer(Name)) of 
        {ok, Pids} ->
            Results = lists:map(fun(Pid) ->
                                        gen_server:call(Pid, Msg)
                                end, Pids),
            case cmkit:distinct(Results) of 
                [Value] -> 
                    Value;
                Other ->
                    {error, Other}
            end;
        {error, E} ->
            {error, #{ bucket => Name,
                       error => E}}

    end.

with_reader_pid(Name, Fun) ->
    case cmbus:closest(reader(Name)) of
        {ok, [Pid]} when is_pid(Pid) ->
            Fun(Pid);
        {error, E} ->
            cmkit:danger({Name, reader, E}),
            {error, E}
    end.

with_tree(Pid, Fun) ->
    {ok, Header, _} = cbt_file:read_header(Pid),
    {_, Root} = Header,
    {ok, Tree} = cbt_btree:open(Root, Pid),
    Fun(Tree).

read(Tree) ->
    read(Tree, {}).

read(Name, Spec) when is_atom(Name) ->
    with_reader_pid(Name, fun(Pid) ->
                                  read(Pid, Spec)
                          end);

read(Pid, Spec) when is_pid(Pid) ->
    with_tree(Pid, fun(T) ->
                           read(T, Spec)
                   end);

read(Tree, Keys) when is_list(Keys) ->
    filter(cbt_btree:lookup(Tree, Keys));

read(Tree, {_, _, _}=K) ->
    filter(cbt_btree:lookup(Tree,[K]));

read(Tree, Spec) ->
    fold(Tree, start_key(Spec), match_fun(Spec)).

read_all(Tree, Specs) ->
    lists:flatten(lists:foldl(fun(S, Acc) ->
                                      [read(Tree, S)|Acc]
                              end, [], Specs)).


read(Name, KSpec, VSpec) when is_atom(Name) ->
    with_reader_pid(Name, fun(Pid) ->
                                  read(Pid, KSpec, VSpec)
                          end);

read(Pid, KSpec, VSpec) when is_pid(Pid) ->
    with_tree(Pid, fun(T) ->
                           read(T, KSpec, VSpec)
                   end);

read(Tree, KSpec, VSpec)  ->
    unwind(Tree, KSpec, VSpec).

filter(Kvs) ->
    lists:foldr(fun({ok, {{S, P, O}, V}}, Acc) ->
                        [{S, P, O, V}|Acc];
                   (_, Acc) ->
                        Acc
                end, [], Kvs).

unwind(Tree, {S, Decoder}) ->
    unwind(Tree, {S}, Decoder);

unwind(Tree, {S, P, Decoder}) ->
    unwind(Tree, {S, P}, Decoder);

unwind(Tree, {S, P, O, Decoder}) ->
    unwind(Tree, {S, P, O}, Decoder).

unwind(Tree, Spec, Decoder) ->
    fold(Tree, start_key(Spec), match_fun(Spec, Decoder)).


fold(Tree, Start, Fun) ->
    {ok, _, Res} = cbt_btree:fold(Tree, fun({{S, P, O} = K, V}, Acc) ->
                                                case Fun(K, V) of 
                                                    true ->
                                                        {ok, [{S, P, O, V}|Acc]};
                                                    false ->
                                                        {ok, Acc};
                                                    stop ->
                                                        {stop, Acc}
                                                end
                                        end, [], [{start_key, Start}]),
    lists:reverse(Res).

start_key({S, P, O}) -> {S, P, O};
start_key({S, P, O, _}) -> {S, P, O};
start_key({S, P}) -> {S, P, 0};
start_key({S}) -> {S, 0, 0};
start_key(_) -> {0, 0, 0}.

match_fun({S, P, O}) ->
    fun({S0, P0, O0}, _) when S0 =:= S andalso P0 =:= P andalso O0 =:= O -> true;
       (_, _) -> false
    end;


match_fun({S, P}) ->
    fun({S0, P0, _}, _) when S0 =:= S 
                             andalso P0 =:= P -> true;
       (_, _) -> false
    end;


match_fun({S}) ->
    fun({S0, _, _}, _) when S0 =:= S -> true;
       (_, _) -> false
    end;

match_fun({S, P, O1, O2}) ->
    fun({S0, P0, O0}, _) when S0 =:= S 
                              andalso P0 =:= P 
                              andalso O0 >= O1 
                              andalso O0 =< O2 -> 

            true;
       (_, _) -> 
            stop
    end;

match_fun({}) ->
    fun(_, _) -> true end;

match_fun(_) ->
    fun(_, _) -> stop end.

match_fun({S, P, O}, Decoder) ->
    fun({S0, P0, O0}, V) when S0 =:= S 
                              andalso P0 =:= P 
                              andalso O0 =:= O ->
            decodes(Decoder, V);
       (_, _) -> 
            false
    end;

match_fun({S, P}, Decoder) ->
    fun({S0, P0, _}, V) when S0 =:= S 
                             andalso P0 =:= P -> 
            decodes(Decoder, V);
       (_, _) -> false
    end;

match_fun({S}, Decoder) ->
    fun({S0, _, _}, V) when S0 =:= S ->
            decodes(Decoder, V);
       (_, _) -> false
    end.

decodes(Decoder, V) ->
    case cmdecode:decode(Decoder, V) of 
        {ok, _} ->
            true;
        _ ->
            false
    end.

all_new(Tree, Entries) ->
    Keys = lists:map(fun({S, P, O, _}) ->
                             {S, P, O}
                     end, Entries),
    case read(Tree, Keys) of 
        [] -> true;
        _ -> false
    end.
