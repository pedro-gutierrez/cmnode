-module(cmtemplate_util).
-export([reload/0, is_template/1]).

is_template(T) ->
    case code:is_loaded(T) of 
        false -> 
            false;
        _ -> 
            lists:member({render, 2}, T:module_info(exports))
    end.

reload() ->   
    spawn(fun() ->
                  reload(cmconfig:templates())
          end).

reload([]) -> ok;
reload([#{ contents := Data, name := Name }|Rem]) ->
    compile(Data, Name),
    reload(Rem).

compile(Template, Mod) ->
    case erlydtl:compile_template(Template, Mod, [{auto_escape, false}]) of 
        {ok, Mod} -> ok;
        {ok, Mod, Warnings} ->
            cmkit:log({template, Mod, Warnings});
        {error, Errors, Warnings} ->
            cmkit:log({template, Mod, Errors, Warnings});
        Other -> 
            cmkit:log({template, Mod, Other})
    end.
