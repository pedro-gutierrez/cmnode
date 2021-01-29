-module(cmcloud).
-export([
         state/0,
         all_ok/1,
         is_local/1,
         current_nodes/0,
         current_peer_nodes/0,
         expected_nodes/0,
         is_clustered/0
        ]).

is_clustered() ->
    Localhost = localhost(),
    case lists:map(fun cmkit:to_bin/1, net_adm:host_file()) of 
        [Localhost] -> false;
        _ -> true
    end.

expected_nodes() -> 
    Sname = erlang:binary_to_list(cmkit:sname()),
    [ erlang:list_to_atom(
        string:join([ Sname,
                      erlang:atom_to_list(H)
                    ], "@")
       ) || H <- net_adm:host_file()].

current_nodes() ->
    ExpectedNodes = expected_nodes(),
    cmkit:intersection(ExpectedNodes, [node()|nodes()]).

current_peer_nodes() ->
    ExpectedNodes = expected_nodes(),
    cmkit:intersection(ExpectedNodes, nodes()).

localhost() -> cmkit:node_host_short(node()).

is_local(Hosts) ->
    Localhost = localhost(), 
    lists:member(Localhost, Hosts).

state() ->
    state(length(current_nodes()), length(expected_nodes())).

state(Nodes, Hosts) when Nodes == Hosts ->
    green;

state(Nodes, Hosts) when Nodes =< Hosts/2 ->
    red;

state(_, _) ->
    yellow.

ok(ok) -> true;
ok({ok, _}) -> true;
ok(_) -> false.

all_ok(Res) ->
    case lists:all(fun ok/1, Res) of
        true -> ok;
        false -> inconsistent
    end.
