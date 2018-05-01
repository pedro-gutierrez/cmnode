-module(cmscheme_compile).
-export([app/1]).

app(#{ init := Init,
       update := Update,
       decoders := Decoders,
       encoders := Encoders,
       effects := Effects }) ->

    {ok, lists:flatten([ init(Init),
           encoders(Encoders),
           decoders(Decoders),
           update(Update),
           effects(Effects),
           cmscheme_ast:call(<<"app">>, lists:map(fun cmscheme_ast:literal/1, [<<"init">>,
                                                                               <<"update">>,
                                                                               <<"decoders">>,
                                                                               <<"encoders">>,
                                                                               <<"effects">> ]))
         ])}.


init(Spec) ->
    cmscheme_ast:def(<<"init">>, term(Spec)).

model(#{ type := object, spec := Spec}) ->
    Args = model(maps:keys(Spec), Spec, []), 
    cmscheme_ast:call(list, Args);


model(#{}) -> 
    cmscheme_ast:call(list, []).

model([], _, Out) -> Out;
model([K|Rem], Spec, Out) ->
    model(Rem, Spec, [term(K, maps:get(K, Spec))|Out]).

cmds(Cmds) ->
    cmscheme_ast:call(list, lists:map(fun cmd/1, Cmds)).

cmd(#{ effect := Effect, encoder := Encoder }) ->
    cmscheme_ast:call(list, [ cmscheme_ast:sym(Effect),
                              cmscheme_ast:sym(Encoder)
                            ]);

cmd(#{ effect := Effect }) ->
    cmscheme_ast:call(list, [ cmscheme_ast:sym(Effect) ]).


condition(#{ type := present, spec := Keys }) ->
    cmscheme_ast:call(list, [ 
                             cmscheme_ast:sym(present),
                             cmscheme_ast:call(list, 
                                               lists:map(fun cmscheme_ast:sym/1, Keys))
                            ]);

condition(#{ type := equal, spec := Spec }) ->
    cmscheme_ast:call(list, [ 
                             cmscheme_ast:sym(equal),
                             cmscheme_ast:call(list, terms(Spec))
                            ]);

condition(#{ type := true }) ->
    cmscheme_ast:call(list, [ cmscheme_ast:sym(true) ]);

condition(#{ type := false }) ->
    cmscheme_ast:call(list, [ cmscheme_ast:sym(false)]).

update(Update) ->
    UpdateAst = update(maps:keys(Update), Update, []),
    cmscheme_ast:def(<<"update">>, cmscheme_ast:call(list, UpdateAst)).

update([], _, Out) -> Out;
update([K|Rem], Spec, Out) ->
    update(Rem, Spec, [term(K, maps:get(K, Spec))|Out]).
                      

encoders(Encoders) ->
    EncodersAst = cmscheme_ast:call(list, terms(maps:keys(Encoders), Encoders, [])),
    cmscheme_ast:def(<<"encoders">>, EncodersAst).


decoders(Decoders) ->
    DecodersAst = cmscheme_ast:call(list, lists:map(fun decoder/1, Decoders)),
    cmscheme_ast:def(<<"decoders">>, DecodersAst).

decoder(#{ msg := Msg, 
           spec := Spec }) ->
    term(Msg, Spec).


term(K, #{ type := object, spec := Spec}) when is_map(Spec) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), 
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(object),
                                                      cmscheme_ast:call(list, terms(Spec))
                                                     ])
                            ]);

term(K, #{ type := keyword, value := V}) when is_atom(V) -> 
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), cmscheme_ast:sym(V)]);

term(K, #{ type := text, value := V  }) when is_binary(V) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), cmscheme_ast:str(V)]);

term(K, #{ type := text, from := From, at := At }) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(text),
                                                      term(#{ from => From,
                                                              at => At })
                                                     ])
                            ]);

term(K, #{ type := text, from := From }) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(text),
                                                      term(#{ from => From })
                                                     ])
                            ]);

term(K, #{ type := text }) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(text),
                                                      cmscheme_ast:sym(any)
                                                     ])
                            ]);

term(K, #{ type := number }) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(number),
                                                      cmscheme_ast:sym(any)
                                                     ])
                            ]);

term(K, #{ type := list, value := List }) when is_list(List) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, 
                                               [ cmscheme_ast:sym(list),
                                                 cmscheme_ast:call(list, lists:map(fun term/1, List))
                                               ])
                            ]);


term(K, #{ type := list, spec := Spec }) when is_map(Spec) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, 
                                               [ cmscheme_ast:sym(list),
                                                    term(Spec)
                                               ])
                            ]);


term(K, #{ type := list }) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(list),
                                                      cmscheme_ast:sym(any)
                                                     ])
                            ]);





term(K, #{ type := view, spec := Spec}) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), term(Spec)]);

term(K, #{ type := effect, class := Class,  name := K,  settings := Settings}) -> 
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(K),
                             cmscheme_ast:sym(Class),
                             cmscheme_ast:call(list, terms(Settings))
                            ]);

term(K, #{ value := V }) when is_binary(V) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), cmscheme_ast:str(V)]);


term(K, V) when is_atom(K) and is_binary(V) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(K), cmscheme_ast:str(V)]);

term(K, V) when is_list(V) ->
    cmscheme_ast:call(list, [
                               cmscheme_ast:sym(K),
                               cmscheme_ast:call(list, terms(V))
                            ]);

term(K, V) ->
    cmscheme_ast:call(list, [
                               cmscheme_ast:sym(K),
                                term(V)
                            ]).


terms(Map) when is_map(Map) ->
    terms(maps:keys(Map), Map, []);

terms(List) when is_list(List) ->
    lists:map(fun term/1, List).

terms([], _, Out) -> lists:reverse(Out);
terms([K|Rem], Terms, Out) ->
    terms(Rem, Terms, [term(K, maps:get(K, Terms))|Out]).

term(#{}=Map) when map_size(Map) == 0-> cmscheme_ast:call(list, []);

term(A) when is_atom(A) -> cmscheme_ast:sym(A);

term(#{ model := Model, condition := Cond, cmds := Cmds }) ->
    cmscheme_ast:call(list, [
                             condition(Cond),
                             model(Model),
                             cmds(Cmds)
                            ]);

term(#{ model := Model, cmds := Cmds }) ->
    cmscheme_ast:call(list, [
                             condition(#{ type => true }),
                             model(Model),
                             cmds(Cmds)
                            ]);


term(#{ tag := Tag, attrs := Attrs, children := Children }) ->
    cmscheme_ast:call(list, [cmscheme_ast:str(Tag),
                             cmscheme_ast:call(list, view_attrs(Attrs)),
                             view_children(Children)
                            ]);

term(#{ view := View, params := Params}) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(view),
                             cmscheme_ast:call(list, [
                                cmscheme_ast:call(list, [cmscheme_ast:sym(name), term(View)]),
                                cmscheme_ast:call(list, [
                                                         cmscheme_ast:sym(params), 
                                                         term(Params)])
                                                     ])
                            ]);


term(#{ encoder := Name }) when is_atom(Name) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(encoder),
                             cmscheme_ast:sym(Name)
                            ]);
    
term(#{ view := View, condition := _}) ->
    cmscheme_ast:call(list, [cmscheme_ast:sym(view),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:call(list, [cmscheme_ast:sym(name), cmscheme_ast:sym(View)])
                                                     ])
                            ]);



term(#{ iterate := From, using := ItemView }) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(iterate),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(name), 
                                                                               cmscheme_ast:sym(ItemView)
                                                                              ]),

                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(items), 
                                                                               term(From)
                                                                              ])
                                        
                                                     ])
                            ]);


term(#{ type := object, spec := Spec}) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(object),
                             cmscheme_ast:call(list, terms(Spec))
                            ]);


term(#{ type := map, spec := #{ options := Options,
                                value := Value}}) ->


    OptionsAst = cmscheme_ast:call(list, [
        cmscheme_ast:sym(options),
        cmscheme_ast:call(list, lists:map(fun(#{ source := Source, target := Target }) ->
                                cmscheme_ast:call(list, [
                                                         cmscheme_ast:call(list, [
                                                                                  cmscheme_ast:sym(source),
                                                                                  term(Source)
                                                                                 ]),
                                                         cmscheme_ast:call(list, [
                                                                                  cmscheme_ast:sym(target),
                                                                                  term(Target)
                                                                                 ])

                                                        ])
                                          end, Options))
                                         ]),
    
    ValueAst = cmscheme_ast:call(list, [
        cmscheme_ast:sym(value),
        term(Value)
                                       ]),

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(map),
                             cmscheme_ast:call(list, [OptionsAst, ValueAst])
                            ]);
                             
                             
term(#{ from := Key, at := At })  -> 
    cmscheme_ast:call(list, [cmscheme_ast:sym(from),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:sym(Key),
                                                      term(At)
                                                     ])
                            ]);

term(#{ from := Key })  -> 
    cmscheme_ast:call(list, [cmscheme_ast:sym(from),
                             cmscheme_ast:sym(Key)
                            ]);

   


term(#{ text := #{ literal := Text}}) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),    
                             cmscheme_ast:str(Text)
                            ]);

term(#{ type := text, from := From, at := At }) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                             term(#{ from => From,
                                        at => At })
                            ]);

term(#{ type := text, from := From }) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                             term(#{ from =>  From })
                            ]);


term(#{ type := text, spec := Spec }) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                             term(Spec)
                            ]);

term(#{ type := text }) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                             cmscheme_ast:sym(any)
                            ]);


term(#{ text := Spec })  -> 
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(text),
                            term(Spec)
                            ]);

term(#{ value := Text}) when is_binary(Text) ->
    term(Text);

term(Text) when is_binary(Text) ->
    cmscheme_ast:call(list, [ 
                             cmscheme_ast:sym(text),    
                             cmscheme_ast:str(Text)
                            ]).

view_attrs(Attrs) when is_map(Attrs) ->
    maps:fold(fun(K, V, Out) ->
                [term(K, V)|Out]
              end, [], Attrs).

view_children(Spec) when is_list(Spec) ->
    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(list),
                             cmscheme_ast:call(list, view_children(Spec, []))
                            ]);

view_children(#{ loop := From, with := View }) ->

    ViewAst = case is_binary(View) of
                  true -> cmscheme_ast:sym(View);
                  false -> term(View)
              end,

    cmscheme_ast:call(list, [
                             cmscheme_ast:sym(loop),
                             cmscheme_ast:call(list, [
                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(items),
                                                                               term(From)
                                                                              ]),
                                                      cmscheme_ast:call(list, [
                                                                               cmscheme_ast:sym(name),
                                                                               ViewAst
                                                                              ])

                                                     ])

                            ]).


view_children([], Out) -> lists:reverse(Out);
view_children([Spec|Rem], Out) ->
    view_children(Rem, [term(Spec)|Out]).

effects(Effects) ->
    EffectsAst = cmscheme_ast:call(list, terms(maps:keys(Effects), Effects, [])),
    cmscheme_ast:def(<<"effects">>, EffectsAst).
