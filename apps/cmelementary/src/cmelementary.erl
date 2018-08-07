-module(cmelementary).
-export([compile/2]).


compile(#{ effects := EffectsSpec,
           init := InitSpec,
           update := UpdateSpecs,
           decoders := DecoderSpecs,
           encoders := EncoderSpecs }, Settings) -> 
    case compile_effects(EffectsSpec, Settings) of 
        {ok, Effects} -> 
            case compile_update(InitSpec, Settings) of 
                {ok, Init} -> 
                    case compile_updates(UpdateSpecs, Settings) of 
                        {ok, Updates} ->
                            case compile_decoders(DecoderSpecs, Settings) of 
                                {ok, Decoders} -> 
                                    case compile_encoders(EncoderSpecs, Settings) of 
                                        {ok, Encoders} -> 
                                            {ok, #{ init => Init,
                                                    update => Updates,
                                                    decoders => Decoders,
                                                    encoders => Encoders,
                                                    effects => Effects }};
                                        Other -> 
                                            Other
                                    end;
                                Other -> 
                                    Other
                            end;
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end.

compile_encoders(Specs, Ctx) ->
    compile_encoders(maps:keys(Specs), Specs, Ctx, #{}).

compile_encoders([], _, _, Out) -> {ok, Out};
compile_encoders([K|Rem], Specs, Ctx, Out) -> 
    case term(maps:get(K, Specs), Ctx) of 
        {ok, Compiled} -> 
            compile_encoders(Rem, Specs, Ctx, Out#{ K => Compiled });
        Other ->
            Other
    end.

compile_decoders(Specs, Ctx) ->
    compile_decoders(Specs, Ctx, #{}).

compile_decoders([], _, Out) -> {ok, Out};
compile_decoders([#{ msg := Msg,
                     spec := Spec }|Rem], Ctx, Out) -> 
    case term(Spec, Ctx) of 
        {ok, Compiled} -> 
            compile_decoders(Rem, Ctx, Out#{ Msg => Compiled });
        Other ->
            Other
    end.


compile_effects(Effects, Settings) -> 
    compile_effects(maps:keys(Effects), Effects, Settings, #{}).

compile_effects([], _, _, Out) -> {ok, Out};
compile_effects([K|Rem], Effects, Settings, Out) ->
    #{ class := Class, 
       name := Name, 
       settings := EffSettingsSpec } = maps:get(K, Effects),
    case cmencode:encode(EffSettingsSpec, #{}, Settings) of 
        {ok, EffSettings} -> 
            compile_effects(Rem, Effects, Settings, Out#{ K => #{ class => Class,
                                                                  name => Name,
                                                                  settings => EffSettings }});
        Other ->
            Other
    end.


compile_updates(Specs, Settings) -> 
    compile_updates(maps:keys(Specs), Specs, Settings, #{}).

compile_updates([], _, _, Out) -> {ok, Out};
compile_updates([K|Rem], Specs, Settings, Out) -> 
    ClauseSpecs = maps:get(K, Specs),
    case compile_update_clauses(ClauseSpecs, Settings, []) of 
        {ok, Clauses} -> 
            compile_updates(Rem, Specs, Settings, Out#{ K => Clauses});
        Other -> 
            Other
    end.

compile_update_clauses([], _, Out) -> {ok, lists:reverse(Out)};
compile_update_clauses([C|Rem], Settings, Out) ->
    case compile_update(C, Settings) of 
        {ok, Update} -> 
            compile_update_clauses(Rem, Settings, [Update|Out]);
        Other -> 
            Other
    end.

compile_update(#{ model := ModelSpec, 
                  cmds := Cmds,
                  condition := ConditionSpec }, Settings) -> 
    case term(ModelSpec, Settings) of 
        {ok, Model} -> 
            case term(ConditionSpec, Settings) of 
                {ok, Condition} -> 

                    {ok, #{ model => Model,
                            cmds => Cmds,
                            condition => Condition }};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

compile_update(#{ model := _, cmds := _} = Spec, Settings) -> 
    compile_update(Spec#{ condition => #{ type => true }}, Settings).


compile_object([], _, _, Out) -> {ok, Out};
compile_object([K|Rem], Spec, Settings, Out) -> 
    case term(maps:get(K, Spec), Settings) of 
        {ok, V} -> 
            compile_object(Rem, Spec, Settings, Out#{ K => V});
        Other -> 
            Other
    end.

terms(Specs, Settings) -> 
    terms(Specs, Settings, []).

terms([], _, Out) -> {ok, lists:reverse(Out)};
terms([Spec|Rem], Settings, Out) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            terms(Rem, Settings, [Compiled|Out]);
        Other -> 
            Other
    end.

term(#{ type := list, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ list => Compiled}};
        Other -> 
            Other
    end;

term(#{ type := object, spec := Spec}, Settings) -> 
    case compile_object(maps:keys(Spec), Spec, Settings, #{}) of 
        {ok, Compiled} -> 
            {ok, #{ object => Compiled}};
        Other -> 
            Other
    end;

term(#{ type := text, key := Key, in := In}, Settings) ->
    case term( #{ key => Key,
                  in => In }, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ text => Compiled}};
        Other -> 
            Other
    end;

    
term(#{ text := Spec }, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ text => Compiled}};
        Other -> 
            Other
    end;

term(#{ literal := V }, _) ->  {ok, V};

term(#{ type := true }, _) -> {ok, true};
term(#{ type := false}, _) -> {ok, false};
term(#{ type := equal, spec := Spec }, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ equal => Compiled}};
        Other -> 
            Other
    end;

term(#{ type := present, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ present => Compiled}};
        Other -> 
            Other
    end;


term(#{ type := list, value := V }, Settings) -> 
    terms(V, Settings);



term(#{ type := format, 
        params := ParamsSpec,
        pattern := PatternSpec }, Settings) -> 
    case term(PatternSpec, Settings) of 
        {ok, Pattern} -> 
            case term(ParamsSpec, Settings) of 
                {ok, Params} -> 
                    {ok, #{ format => #{ pattern => Pattern,
                                         params => Params }}};
                Other -> 
                    Other
            end;
        Other ->
            Other
    end;


term(#{ type := percentage,
        den := DenSpec,
        num := NumSpec }, Settings) -> 
    case term(DenSpec, Settings) of
        {ok, Den} -> 
            case term(NumSpec, Settings) of 
                {ok, Num} -> 
                    {ok, #{ percent => #{ num => Num,
                                          den => Den }}};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ map := #{ center := CenterSpec,
                  id := IdSpec,
                  markers := MarkersSpec,
                  style := StyleSpec,
                  zoom := ZoomSpec }}, Settings) -> 
    case term(CenterSpec, Settings) of 
        {ok, Center} -> 
            case term(IdSpec, Settings) of 
                {ok, Id} -> 
                    case term(MarkersSpec, Settings) of 
                        {ok, Markers} -> 
                            case term(StyleSpec, Settings) of 
                                {ok, Style} -> 
                                    case term(ZoomSpec, Settings) of 
                                        {ok, Zoom} -> 
                                            {ok, #{ map => #{ center => Center,
                                                              id => Id,
                                                              markers => Markers,
                                                              style => Style,
                                                              zoom => Zoom }}};
                                        Other -> 
                                            Other
                                    end;
                                Other -> 
                                    Other
                            end;
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ maybe := Spec}, Settings) -> 
    case term(Spec, Settings) of
        {ok, Term} -> 
            {ok, #{ maybe => Term }};
        Other -> 
            Other
    end;

term(#{ date := #{ format := FormatSpec,
                   value := ValueSpec }}, Settings) -> 
    case term(FormatSpec, Settings) of 
        {ok, Format} -> 
            case term(ValueSpec, Settings) of 
                {ok, Value} -> 
                    {ok, #{ date => #{ format => Format,
                                       value => Value }}};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ json := SourceSpec,
        indent := IndentSpec }, Settings) -> 
    case term(SourceSpec, Settings) of 
        {ok, Source} -> 
            case term(IndentSpec, Settings) of 
                {ok, Indent} -> 
                    {ok, #{ json => #{ source => Source,
                                       indent => Indent }}};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;




term(#{ value := V}, _) ->  {ok, V};

term(#{ one_of := Options }, Settings) -> 
    case terms(Options, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ one_of => Compiled }};
        Other -> 
            Other
    end;

term(#{ type := merged_list, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Terms} -> 
            {ok, #{ merged_list => Terms }};
        Other -> 
            Other
    end;

term(List, Settings) when is_list(List) -> 
    terms(List, Settings);

term(#{ type := view, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ view => Compiled }};
        Other -> 
            Other
    end;

term(#{ tag := Tag, attrs := AttrsSpec, children := ChildrenSpecs}, Settings) -> 
    case term(#{ type => object,
                 spec => AttrsSpec }, Settings) of 
        {ok, Attrs} ->
            case term(ChildrenSpecs, Settings) of 
                {ok, Children} -> 
                    {ok, #{ tag => Tag,
                            attrs => Attrs,
                            children => Children }};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ view := View, params := ParamsSpec}, Settings) -> 
    case term(ParamsSpec, Settings) of 
        {ok, Params} -> 
            {ok, #{ name => View,
                    params => Params }};
        Other -> 
            Other
    end;

term(#{ loop := LoopSpec,
        context := ContextSpec,
        with := ViewSpec }, Settings) -> 
    case term(LoopSpec, Settings) of 
        {ok, Loop} -> 
            case term(ContextSpec, Settings) of 
                {ok, Context} -> 
                    case term(ViewSpec, Settings) of 
                        {ok, View} -> 
                            {ok, #{ loop => Loop,
                                    context => Context,
                                    with => View}};
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ type := either,
        options := Specs}, Settings) -> 

    case terms(Specs, Settings) of 
        {ok, Options} -> 
            {ok, #{ either => Options }};
        Other -> 
            Other
    end;

term(#{ type := condition,
        spec := Spec,
        condition := ConditionSpec }, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Result} -> 
            case term(ConditionSpec, Settings) of 
                {ok, Condition} -> 
                    {ok, #{ 'when' => Condition,
                            'then' => Result }};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ key := K, in := InSpec }, Settings) ->
    case term(InSpec, Settings) of 
        {ok, In} -> 
            {ok, #{ key =>  K,
                    in => In}};
        Other -> 
            Other
    end;

term(#{ key := K }, _) ->
    {ok, #{ key => K }};

term(object, _) -> 
    {ok, #{ type => object }};

term(#{ value := V}, _) -> {ok, V};

term(#{ type := number }, _) -> 
    {ok, #{ any => number }};

term(#{ type := text }, _) -> 
    {ok, #{ any => text }};

term(#{ type := file }, _) -> 
    {ok, #{ any => file }};

term(V, _) when is_atom(V) or is_number(V) -> 
    {ok, V};

term(Map, _) when is_map(Map) andalso map_size(Map) =:= 0 ->
    {ok, #{}};

term(Other, _) -> 
    cmkit:warning({cmelementary, term, other, Other}),
    {ok, Other}.
