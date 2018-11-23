-module(cmeffect).
-export([effects/0, effect/1, reload/0]).


reload() ->
    Mods = cmcode:load_beams(cmeffect, "cmeffect_"),
    cmkit:log({cmconfig, effects, length(Mods)}),
    I = lists:foldl(fun(Mod, Index) ->
                            case cmkit:implements(Mod, [{effect_info,0}]) of 
                                true -> 
                                    Name = Mod:effect_info(),
                                    I2 = Index#{ Name => Mod },
                                    cmkit:set_app_env(cmeffect, Name, Mod),
                                    I2;
                                false -> 
                                    Index
                            end
                end, #{}, Mods),
    cmkit:set_app_env(cmeffect, index, I),
    I.

effects() ->
    cm_effect_index:get().

effect(N) ->
    cmkit:app_env(cmeffect, N).
