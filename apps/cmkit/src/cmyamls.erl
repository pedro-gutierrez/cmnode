-module(cmyamls).
-export([all/0, of_type/1, of_cat/1, of_type_cat/2, of_type_name/2]).

filenames() ->
    cmkit:files(cmkit:etc(), ".yml").

all() ->
    lists:map(fun(Filename) ->
                      {yaml, Filename, cmkit:yaml(Filename)}     
              end, filenames()).

of_type_name(Type, Name) ->
    TypeBin = cmkit:to_bin(Type),
    NameBin = cmkit:to_bin(Name),
    specs_from(lists:filter(fun({yaml, _, {ok, #{ <<"type">> := Type2, <<"name">> := Name2}}}) -> 
                         (Type2 =:= TypeBin) and (Name2 =:= NameBin) ;
                    (_) -> false end, all())).

of_type(Type) ->
    TypeBin = cmkit:to_bin(Type),
    specs_from(lists:filter(fun({yaml, _, {ok, #{ <<"type">> := Type2}}}) -> 
                         Type2 =:= TypeBin;
                    (_) -> false end, all())).

of_cat(Cat) ->
    CatBin = cmkit:to_bin(Cat),
    specs_from(lists:filter(fun({yaml, _, {ok, #{ <<"category">> := Cat2}}}) -> 
                         Cat2 =:= CatBin;
                    (_) -> false end, all())).

of_type_cat(Type, Cat) ->
    TypeBin = cmkit:to_bin(Type),
    CatBin = cmkit:to_bin(Cat),
    specs_from(lists:filter(fun({yaml, _, {ok, #{ <<"type">> := Type2, <<"category">> := Cat2}}}) -> 
                         (Type2 =:= TypeBin) and (Cat2 =:= CatBin) ;
                    (_) -> false end, all())).


specs_from(List) ->
    lists:map(fun spec_from/1, List).

spec_from({yaml, _, {ok, Spec}}) ->
    {ok, Spec}.
