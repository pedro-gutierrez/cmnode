-module(cmeffect).
-export([effects/0, effect/1, reload/0]).


reload() ->
    Mods = cmcode:load_beams(cmeffect, "cmeffect_"),
    cmkit:log({cmconfig, effects, length(Mods)}),
    I = lists:foldl(fun(Mod, Index) ->
                            case cmkit:implements(Mod, [{effect_info,0}]) of 
                                true -> 
                                    Name = Mod:effect_info(),
                                                %NameBin = cmkit:to_bin(Name),
                                                %I2 = Index#{ Name => Mod, NameBin => Mod },
                                    I2 = Index#{ Name => Mod },
                                    cmkit:set_app_env(cmeffect, Name, Mod),
                                                %cmkit:set_app_env(cmeffect, NameBin, Mod),
                                    I2;
                                false -> 
                                    Index
                            end
                    end, #{}, Mods),
    cmkit:set_app_env(cmeffect, index, I),
    I.

effects() ->
    cmkit:app_env(cmeffect, index).

effect(N) ->
    cmkit:app_env(cmeffect, N).
