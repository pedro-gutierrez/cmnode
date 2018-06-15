-module(cmeval).
-export([eval/2, eval/3, eval/4]).

eval(Spec, In) ->
    eval(Spec, #{}, In, #{}).

eval(Spec, In, Config) ->
    eval(Spec, #{}, In, Config).

eval(#{ type := true }, _, _, _) -> true;
eval(#{ type := false }, _, _, _) -> false;

eval(#{ type := equal, 
        spec := Specs }, _, In, Config) when is_list(Specs) -> 
    all_equal(lists:map(fun(Spec) ->
                                cmencode:encode(Spec, In, Config)
                        end, Specs));

eval(#{ type := greater_than } = Spec, _, In, Config) -> 
    case cmencode:encode(Spec, In, Config) of
        {ok, V} -> V;
        Other -> 
            cmkit:log({cmeval, error, Spec, Other}),
            false
    end;

eval(#{ type := 'and', 
        spec := Specs }, _, In, Config) when is_list(Specs) -> 
    all_true(lists:map(fun(Spec) ->
                                eval(Spec, In, Config)
                        end, Specs));

eval(#{ type := member }=Spec, _, In, Config) ->
    case cmencode:encode(Spec, In, Config) of 
        {ok, true} -> true;
        {ok, false} -> false;
        Other ->
            cmkit:log({cmeval, error, Spec, Other}),
            false
    end;


eval(#{ type := present, spec := Keys}, _, In, _) when is_map(In) ->
    cmkit:has_all_keys(Keys, In);


eval(#{ encoder := Name}, Encs, In, Config) when is_atom(Name) ->
    case maps:get(Name, Encs, undef) of 
        undef -> 
             cmkit:log({cmeval, encoder, unknown, Name}),
             false;
        Enc ->
            eval(Enc, #{}, In, Config)
    end;

eval(Spec, _, In, Config) ->
    case cmencode:encode(Spec, In, Config) of
        {ok, true} -> true;
        {ok, false} -> false;
        {ok, Other} ->
            cmkit:danger({cmeval, non_boolean_result, Spec, Other}),
            false;
        Other ->
            cmkit:danger({cmeval, error, Spec, Other}),
            false
    end.


all_equal([V|Rem]) -> all_equal(Rem, V).
all_equal([], _) -> true;
all_equal([V|Rem], V) -> all_equal(Rem, V);
all_equal(_, _) -> false.

all_true([]) -> true;
all_true([true|Rem]) -> all_true(Rem);
all_true([_|_]) -> false.

