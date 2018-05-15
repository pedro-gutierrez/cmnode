-module(cmeval).
-export([eval/4]).

eval(#{ type := true }, _, _, _) -> true;
eval(#{ type := false }, _, _, _) -> false;

eval(#{ type := equal, 
        spec := Specs }, _, In, Config) when is_list(Specs) -> 
    all_equal(lists:map(fun(Spec) ->
                                cmencode:encode(Spec, In, Config)
                        end, Specs));

eval(#{ type := member }=Spec, _, In, Config) ->
    case cmencode:encode(Spec, In, Config) of 
        {ok, true} -> true;
        {ok, false} -> false;
        Other ->
            cmkit:log({cmeval, error, Spec, Other}),
            false
    end;



%%eval(#{ type := member, 
%%        spec := Spec }, _, In, Config) -> 
%%    case lists_members_specs(Spec, In) of 
%%        {ok, Spec2} -> 
%%            case cmdecode:decode(#{ type => object,
%%                                    spec => Spec2 }, In, Config) of
%%                {ok, _} -> true;
%%                _ -> false
%%            end;
%%        _ -> false
%%    end;

eval(#{ type := present, spec := Keys}, _, In, _) when is_map(In) ->
    cmkit:has_all_keys(Keys, In);


eval(#{ encoder := Name}, Encs, In, Config) when is_atom(Name) ->
    case maps:get(Name, Encs, undef) of 
        undef -> 
             cmkit:log({cmeval, encoder, unknown, Name}),
             false;
        Enc ->
            case cmencode:encode(Enc, In, Config) of
                {ok, true} -> true;
                {ok, false} -> false;
                Other ->
                    cmkit:log({cmeval, encoder, non_bool, Name, Enc, Other}),
                    false
            end
    end;

eval(Spec, _, _, _) -> 
    cmkit:log({cmeval, not_implemented, Spec}),
    false.

%lists_members_specs(Spec, In) ->
%    lists_members_specs(maps:keys(Spec), Spec, In, #{}).
%
%lists_members_specs([], _, _, Out) -> {ok, Out};
%lists_members_specs([K|Rem], Spec, In, Out) ->
%    ValueSpec = maps:get(K, Spec),
%    case cmencode:encode(ValueSpec) of 
%        {ok, Value} ->
%            lists_members_specs(Rem, Spec, In,
%                                maps:put(K, #{ type => list,
%                                               with => Value }, Out));
%        Other -> Other
%    end.

all_equal([V|Rem]) -> all_equal(Rem, V).
all_equal([], _) -> true;
all_equal([V|Rem], V) -> all_equal(Rem, V);
all_equal(_, _) -> false.


