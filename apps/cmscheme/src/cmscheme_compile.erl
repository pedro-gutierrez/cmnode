-module(cmscheme_compile).
-export([app/2]).

app(#{ init := Init,
       update := Update,
       decoders := Decoders,
       encoders := Encoders,
       effects := Effects }, Settings) ->

    {ok, lists:flatten([ init(Init, Settings),
           encoders(Encoders, Settings),
           decoders(Decoders, Settings),
           update(Update, Settings),
           effects(Effects, Settings),
           cmscheme_ast:call(<<"app">>, lists:map(fun cmscheme_ast:literal/1, [<<"init">>,
                                                                               <<"update">>,
                                                                               <<"decoders">>,
                                                                               <<"encoders">>,
                                                                               <<"effects">> ]))
         ])}.


init(Spec, Settings) ->
    cmscheme_ast:def(<<"init">>, term(Spec, Settings)).

model(#{ type := object, spec := Spec}, Settings) ->
    Args = model(maps:keys(Spec), Spec, Settings, []), 
    cmscheme_ast:call(list, Args);


model(#{}, _) -> 
    cmscheme_ast:call(list, []).

model([], _, _, Out) -> Out;
model([K|Rem], Spec, Settings, Out) ->
    model(Rem, Spec, Settings, [term(K, maps:get(K, Spec), Settings)|Out]).

cmds(Cmds, Settings) ->
    cmscheme_ast:call(list, lists:map(fun(C) ->
                                         cmd(C, Settings)     
                                      end, Cmds)).

cmd(#{ effect := Effect, encoder := Encoder }, _) ->
    cmscheme_ast:call(list, [ cmscheme_ast:sym(Effect),
                              cmscheme_ast:sym(Encoder)
                            ]);

cmd(#{ effect := Effect }, _) ->
    cmscheme_ast:call(list, [ cmscheme_ast:sym(Effect) ]).


condition(#{ type := present, spec := Keys }, _) ->
    cmscheme_ast:call(list, [ 
                             cmscheme_ast:sym(present),
                             cmscheme_ast:call(list, 
                                               lists:map(fun cmscheme_ast:sym/1, Keys))
                            ]);

condition(#{ type := equal, spec := Spec }, Settings) ->
    cmscheme_ast:call(list, [ 
                             cmscheme_ast:sym(equal),
                             cmscheme_ast:call(list, terms(Spec, Settings))
                            ]);

condition(#{ type := true }, _) ->
    cmscheme_ast:call(list, [ cmscheme_ast:sym(true) ]);

condition(#{ type := false }, _) ->
    cmscheme_ast:call(list, [ cmscheme_ast:sym(false)]).

update(Update, Settings) ->
    UpdateAst = update(maps:keys(Update), Update, Settings, []),
    cmscheme_ast:def(<<"update">>, cmscheme_ast:call(list, UpdateAst)).

update([], _, _, Out) -> Out;
update([K|Rem], Spec, Settings, Out) ->
    update(Rem, Spec, Settings, [term(K, maps:get(K, Spec), Settings)|Out]).
                      

encoders(Encoders, Settings) ->
    EncodersAst = cmscheme_ast:call(list, terms(maps:keys(Encoders), Encoders, Settings, [])),
    cmscheme_ast:def(<<"encoders">>, EncodersAst).


decoders(Decoders, Settings) ->
    DecodersAst = cmscheme_ast:call(list, lists:map(fun(D) ->
                                                        decoder(D, Settings)    
                                                    end, Decoders)),
    cmscheme_ast:def(<<"decoders">>, DecodersAst).

decoder(#{ msg := Msg, 
           spec := Spec }, Settings) ->
    term(Msg, Spec, Settings).



term(K, #{ type := object, spec := Spec}, Settings) when is_map(Spec) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), 
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(object),
                                                      cmscheme_ast:call(list, terms(Spec, Settings))
                                                     ])
                            ]);


term(K, #{ type := keyword, value := V}, Settings) when is_atom(V) -> 
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), term(V, Settings)]);

term(K, #{ type := text, value := V  }, _) when is_binary(V) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), cmscheme_ast:str(V)]);

term(K, #{ type := text, key := Key, in := In }, Settings) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(text),
                                                      term(#{ key=> Key,
                                                              in => In }, Settings)
                                                     ])
                            ]);

term(K, #{ type := text, key := Key }, Settings) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(text),
                                                      term(#{ key => Key }, Settings)
                                                     ])
                            ]);

term(K, #{ type := text }, _) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(text),
                                                      cmscheme_ast:sym(any)
                                                     ])
                            ]);


term(K, #{ type := list, value := List }, Settings) when is_list(List) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, 
                                               [ cmscheme_ast:sym(list),
                                                 cmscheme_ast:call(list, lists:map(fun(T) ->
                                                                                      term(T, Settings)     
                                                                                   end, List))
                                               ])
                            ]);


term(K, #{ type := list, spec := Spec }, Settings) when is_map(Spec) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, 
                                               [ cmscheme_ast:sym(list),
                                                    term(Spec, Settings)
                                               ])
                            ]);

term(K, #{ type := view, spec := Spec}, Settings) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), term(Spec, Settings)]);

term(K, #{ type := effect, class := Class,  name := K,  settings := EffectSettings}, Settings) -> 
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(K),
                             cmscheme_ast:sym(Class),
                             cmscheme_ast:call(list, terms(EffectSettings, Settings))
                            ]);

term(K, #{ value := V }, _) when is_binary(V) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), cmscheme_ast:str(V)]);


term(K, V, _) when is_atom(K) and is_binary(V) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), cmscheme_ast:str(V)]);

term(K, V, Settings) when is_list(V) ->
    cmscheme_ast:call(list, [
                               cmscheme_ast:sym(K),
                               cmscheme_ast:call(list, terms(V, Settings))
                            ]);

term(K, V, Settings) ->
    cmscheme_ast:call(list, [
                               cmscheme_ast:sym(K),
                                term(V, Settings)
                            ]).


terms(Map, Settings) when is_map(Map) ->
    terms(maps:keys(Map), Map, Settings, []);

terms(List, Settings) when is_list(List) ->
    lists:map(fun(Term) -> 
                      term(Term, Settings)
              end, List).

terms([], _, _, Out) -> lists:reverse(Out);
terms([K|Rem], Terms, Settings, Out) ->
    terms(Rem, Terms, Settings, [term(K, maps:get(K, Terms),Settings)|Out]).

term(#{}=Map, _) when map_size(Map) == 0-> cmscheme_ast:call(list, []);

term(A, _) when is_atom(A) -> cmscheme_ast:sym(A);

term(#{ type := config, spec := Spec}, Settings) -> 
    case cmencode:encode(Spec, Settings) of 
        {ok, V} when is_number(V) ->
            cmscheme_ast:number(V);
        {ok, V} when is_atom(V) ->
            cmscheme_ast:sym(V);
        {ok, V} -> 
            cmscheme_ast:str(cmkit:to_bin(V));
        Other -> 
            cmkit:danger({cmscheme, compile, config, Spec, Settings, Other}),
            cmscheme_ast:sym(invalid_setting)
    end;

term(#{ model := Model, condition := Cond, cmds := Cmds }, Settings) ->
    cmscheme_ast:call(list, [
                             condition(Cond, Settings),
                             model(Model, Settings),
                             cmds(Cmds, Settings)
                            ]);

term(#{ model := Model, cmds := Cmds }, Settings) ->
    cmscheme_ast:call(list, [
                             condition(#{ type => true }, Settings),
                             model(Model, Settings),
                             cmds(Cmds, Settings)
                            ]);

term(#{ tag := Tag, attrs := Attrs, children := Children }, Settings) ->
    cmscheme_ast:call(list, [cmscheme_ast:str(Tag),
                             cmscheme_ast:call(list, view_attrs(Attrs, Settings)),
                             view_children(Children, Settings)
                            ]);


term(#{ view := View }=Spec, Settings) ->

    NameAst = cmscheme_ast:call(list, [cmscheme_ast:sym(name), term(View, Settings)]),
    ParamsAst = case maps:get(params, Spec, undef) of 
                    undef -> [];
                    ParamsSpec -> 
                        [cmscheme_ast:call(list, [
                                         cmscheme_ast:sym(params),
                                         term(ParamsSpec, Settings)
                                        ])]
                end,

    ConditionAst = case maps:get(condition, Spec, undef) of 
                       undef -> [];
                       ConditionSpec -> 
                            [cmscheme_ast:call(list, [
                                                    cmscheme_ast:sym(condition), 
                                                    term(ConditionSpec, Settings)
                                                   ])]
                    end,

    cmscheme_ast:call(list, [cmscheme_ast:sym(view),
                             cmscheme_ast:call(list, [ NameAst ] ++ ParamsAst ++ ConditionAst)
                            ]);

term(#{ encoder := Name }, _) when is_atom(Name) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(encoder),
                             cmscheme_ast:sym(Name)
                            ]);

term(#{ json := Source,
        indent := Indent }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(json),
                             cmscheme_ast:call(list, [ 
                                cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(source),
                                                       term(Source, Settings)]),
                                cmscheme_ast:call(list, [ cmscheme_ast:sym(indent),
                                                       term(Indent, Settings)])
                                                     ])]);

term(#{ map := #{ id := Id,
                  style := Style,
                  zoom := Zoom,
                  center := Center,
                  markers := Markers }}, Settings) ->

                   cmscheme_ast:call(list, [ cmscheme_ast:sym(map),
                                             cmscheme_ast:call(list, [
                                                                      cmscheme_ast:call(list, [
                                                                                               cmscheme_ast:sym(id), term(Id, Settings) ]),
                                                                      cmscheme_ast:call(list, [
                                                                                               cmscheme_ast:sym(style), cmscheme_ast:sym(Style) ]),

                                                                      cmscheme_ast:call(list, [
                                                                                               cmscheme_ast:sym(zoom), cmscheme_ast:number(Zoom) ]),

                                                                      cmscheme_ast:call(list, [
                                                                                               cmscheme_ast:sym(center), term(Center, Settings) ]),
                                                                      cmscheme_ast:call(list, [
                                                                                                 cmscheme_ast:sym(markers), cmscheme_ast:call(list, terms(Markers, Settings)) ]) 
                                                                     ])]);



term(#{ timestamp := #{ format := Format,
                        value := Value}}, Settings) -> 

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(timestamp),
                             cmscheme_ast:call(list, [ 
                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(format),
                                                                               term(Format, Settings)]),
                                                      cmscheme_ast:call(list, [ cmscheme_ast:sym(value),
                                                                                term(Value, Settings)])
                                                     ])]);
term(#{ date := #{ format := Format,
                   value := Value}}, Settings) -> 

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(date),
                             cmscheme_ast:call(list, [ 
                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(format),
                                                                               term(Format, Settings)]),
                                                      cmscheme_ast:call(list, [ cmscheme_ast:sym(value),
                                                                                term(Value, Settings)])
                                                     ])]);

term(#{ iterate := From, using := ItemView }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(iterate),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(name), 
                                                                               cmscheme_ast:sym(ItemView)
                                                                              ]),

                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(items), 
                                                                               term(From, Settings)
                                                                              ])
                                        
                                                     ])
                            ]);


term(#{ maybe := Spec }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(maybe),
                             term(Spec, Settings)
                            ]);

term(#{  type := either, 
         options := Options }, Settings) ->

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(either),
                             cmscheme_ast:call(list, lists:map(fun(T) -> 
                                                                       term(T, Settings)
                                                                       end, Options))
                            ]);


term(#{ type := condition,
        condition := Cond,
        spec := Spec }, Settings) ->

    cmscheme_ast:call(list, [
                             
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(condition),
                                                      condition(Cond, Settings)
                                                     ]),

                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(spec),
                                                      term(Spec, Settings)
                                                     ])

                            ]);
    




term(#{ type := merge, spec := Specs }, Settings) -> 
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(merge),
                             cmscheme_ast:call(list, terms(Specs, Settings))
                            ]);



term(#{ type := object, spec := Spec}, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(object),
                             cmscheme_ast:call(list, terms(Spec, Settings))
                            ]);

term(#{ type := object }, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(object),
                             cmscheme_ast:sym(any)
                            ]);


term(#{ type := file, spec := Spec }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(file),
                             term(Spec, Settings)
                            ]);


term(#{ type := entries, spec := Spec }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(entries),
                             term(Spec, Settings)
                            ]);
term(#{ type := files, 
        spec := Spec }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(files),
                             term(Spec, Settings)
                            ]);

term(#{ type := file }, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(file),
                             cmscheme_ast:sym(any)
                            ]);


term(#{ type := format, 
        pattern := Pattern,
        params := Params }, Settings) ->

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(format),
                             cmscheme_ast:call(list, [
                                               cmscheme_ast:call(list, [ 
                                                                        cmscheme_ast:sym(pattern),
                                                                        term(Pattern, Settings) 
                                                                       ]),

                                               cmscheme_ast:call(list, [ 
                                                                        cmscheme_ast:sym(params),
                                                                        cmscheme_ast:call(list, terms(Params, Settings))
                                                                       ])

                                                    ])
                            ]);



term(#{ type := map, spec := #{ options := Options,
                                value := Value}}, Settings) ->


    OptionsAst = cmscheme_ast:call(list, [
        cmscheme_ast:sym(options),
        cmscheme_ast:call(list, lists:map(fun(#{ source := Source, target := Target }) ->
                                cmscheme_ast:call(list, [
                                                         cmscheme_ast:call(list, [
                                                                                  cmscheme_ast:sym(source),
                                                                                  term(Source, Settings)
                                                                                 ]),
                                                         cmscheme_ast:call(list, [
                                                                                  cmscheme_ast:sym(target),
                                                                                  term(Target, Settings)
                                                                                 ])

                                                        ])
                                          end, Options))
                                         ]),
    
    ValueAst = cmscheme_ast:call(list, [
        cmscheme_ast:sym(value),
        term(Value, Settings)
                                       ]),

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(map),
                             cmscheme_ast:call(list, [OptionsAst, ValueAst])
                            ]);


term(#{ one_of := Specs }, Settings) when is_list(Specs) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(one_of),
                             cmscheme_ast:call(list, lists:map(fun(S) ->
                                                                   term(S, Settings)    
                                                                end, Specs))
                            ]);

                             
term(#{ key := Key, in := At }, Settings)  -> 
    cmscheme_ast:call(list, [cmscheme_ast:sym(from),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(Key),
                                                      term(At, Settings)
                                                     ])
                            ]);

term(#{ key := Key }, _)  -> 
    cmscheme_ast:call(list, [cmscheme_ast:sym(from),
                             cmscheme_ast:sym(Key)
                            ]);

   
term(#{ type := boolean }, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(boolean),    
                             cmscheme_ast:sym(any)
                            ]);


term(#{ type := keyword, value := V}, _) when is_atom(V) ->
    cmscheme_ast:sym(V);

term(#{ text := #{ literal := Text}}, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),    
                             cmscheme_ast:str(Text)
                            ]);

term(#{ type := text, value := Text}, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),    
                             cmscheme_ast:str(Text)
                            ]);

term(#{ type := text, key := Key, in := In }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                             term(#{ key => Key,
                                     in => In }, Settings)
                            ]);

term(#{ type := text, key := Key }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                             term(#{ key =>  Key }, Settings)
                            ]);


term(#{ type := text, spec := Spec }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                             term(Spec, Settings)
                            ]);

term(#{ type := text }, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                             cmscheme_ast:sym(any)
                            ]);

term(#{ type := number, value := V }, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(number),
                             cmscheme_ast:number(V)
                            ]);


term(#{ type := number }, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(number),
                             cmscheme_ast:sym(any)
                            ]);

term(#{ type := data }, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(data),
                             cmscheme_ast:sym(any)
                            ]);

term(#{ text := Spec }, Settings)  -> 
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                            term(Spec, Settings)
                            ]);

term(#{ value := Text}, Settings) when is_binary(Text) ->
    term(Text, Settings);


term(#{ type := is_set, spec := Spec }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(is_set),
                             term(Spec, Settings)
                            ]);

term(#{ type := 'not', spec := Spec}, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym('not'),
                             term(Spec, Settings)
                            ]);

term(#{ type := equal, spec := Specs}, Settings) when is_list(Specs) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(equal),
                             cmscheme_ast:call(list, lists:map(fun(S) ->
                                                                       term(S, Settings)
                                                               end, Specs))
                            ]);


term(#{ type := sum, spec := Operands }, Settings) when is_list(Operands) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(sum),
                             cmscheme_ast:call(list, lists:map(fun(S) ->
                                                                       term(S, Settings)
                                                               end, Operands))
                            ]);

term(#{ type := ratio, 
        num := Num,
        den := Den }, Settings)  ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(ratio),
                             cmscheme_ast:call(list, [
                                cmscheme_ast:call(list, [ cmscheme_ast:sym(num), term(Num, Settings) ]),
                                cmscheme_ast:call(list, [ cmscheme_ast:sym(den), term(Den, Settings)])

                                                     ])
                            ]);

term(#{ type := percentage, 
        num := Num,
        den := Den }, Settings)  ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(percentage),
                             cmscheme_ast:call(list, [
                                cmscheme_ast:call(list, [ cmscheme_ast:sym(num), term(Num, Settings) ]),
                                cmscheme_ast:call(list, [ cmscheme_ast:sym(den), term(Den, Settings)])

                                                     ])
                            ]);

term(#{ type := list, size := Size }, _) ->
    cmscheme_ast:call(list, 
                      [ cmscheme_ast:sym(list),
                        cmscheme_ast:number(Size)
                      ]);

term(#{ type := list, value := Specs }, Settings) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(list),
                             cmscheme_ast:call(list, terms(Specs, Settings))
                            ]);

term(#{ type := list, spec := Spec }, Settings) when is_map(Spec) ->
    cmscheme_ast:call(list,[ cmscheme_ast:sym(list),
                             term(Spec, Settings)
                           ]);


term(#{ type := list }, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(list),
                             cmscheme_ast:sym(any)
                            ]);

term(#{ size := Size }, _) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(size),
                             cmscheme_ast:number(Size)
                            ]);

term(Text, _) when is_binary(Text) ->
    cmscheme_ast:call(list, [ 
                             cmscheme_ast:sym(text),    
                             cmscheme_ast:str(Text)
                            ]).

view_attrs(Attrs, Settings) when is_map(Attrs) ->
    maps:fold(fun(K, V, Out) ->
                [term(K, V, Settings)|Out]
              end, [], Attrs).

view_children(Spec, Settings) when is_list(Spec) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(list),
                             cmscheme_ast:call(list, view_children(Spec, Settings, []))
                            ]);


view_children(#{ type := merged_list, spec := Specs}, Settings) -> 

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym('merged-list'),
                             cmscheme_ast:call(list, lists:map(fun(Spec) -> 
                                                                       view_children(Spec, Settings)
                                                               end, Specs))]);

view_children(#{ type := list, value := Specs}, Settings) -> 

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(list),
                             cmscheme_ast:call(list, terms(Specs, Settings))]);


view_children(#{ loop := From, context := Context, with := View }, Settings) ->

    ViewAst = case is_binary(View) of
                  true -> cmscheme_ast:sym(View);
                  false -> term(View, Settings)
              end,

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(loop),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(items),
                                                                               term(From, Settings)
                                                                              ]),
                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(name),
                                                                               ViewAst
                                                                              ]),

                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(context),
                                                                               term(Context, Settings)
                                                                              ])

                                                     ])

                            ]);


view_children(#{ loop := _,  with := _}=Spec, Settings) ->
    view_children(Spec#{ context => #{} }, Settings).


view_children([], _, Out) -> lists:reverse(Out);
view_children([Spec|Rem], Settings, Out) ->
    view_children(Rem, Settings, [term(Spec, Settings)|Out]).

effects(Effects, Settings) ->
    EffectsAst = cmscheme_ast:call(list, terms(maps:keys(Effects), Effects, Settings, [])),
    cmscheme_ast:def(<<"effects">>, EffectsAst).
