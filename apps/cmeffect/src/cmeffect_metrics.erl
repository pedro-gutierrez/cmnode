-module(cmeffect_metrics).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> metrics.

effect_apply([], _) -> ok;

effect_apply([Op|Rem], Pid) ->
    record(Op),
    effect_apply(Rem, Pid).

record(#{ metric := _,
          increment := 0 }) -> ok;

record(#{ metric := Name,
          increment := Value }) ->

    cmmetrics:increment(Name, Value);

record(#{ metric := Name,
          set := Value }) ->

    cmmetrics:set(Name, Value).

