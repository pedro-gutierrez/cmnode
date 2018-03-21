-module(cmyamls).
-export([all/0, of_type/1, of_cat/1, of_type_cat/2]).

filenames() ->
    cmkit:files(cmkit:etc(), ".yml").

all() ->
    lists:map(fun(File) ->
        case file:read_file(File) of
            {ok, Data} ->
                case cmkit:yamld(Data) of 
                    {ok, [Yaml]} -> {ok, Yaml};
                    Other -> Other
                end;
            Other -> Other
        end
    end, filenames()).

of_type(Type) ->
    TypeBin = cmkit:to_bin(Type),
    lists:filter(fun({ok, #{ <<"type">> := Type2}}) -> 
                         Type2 =:= TypeBin;
                    (_) -> false end, all()).

of_cat(Cat) ->
    CatBin = cmkit:to_bin(Cat),
    lists:filter(fun({ok, #{ <<"category">> := Cat2}}) -> 
                         Cat2 =:= CatBin;
                    (_) -> false end, all()).

of_type_cat(Type, Cat) ->
    TypeBin = cmkit:to_bin(Type),
    CatBin = cmkit:to_bin(Cat),
    lists:filter(fun({ok, #{ <<"type">> := Type2, <<"category">> := Cat2}}) -> 
                         (Type2 =:= TypeBin) and (Cat2 =:= CatBin) ;
                    (_) -> false end, all()).
