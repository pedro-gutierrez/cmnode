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
                            case compile_effect_decoders(DecoderSpecs, Settings) of 
                                {ok, Decoders} -> 
                                    case compile_encoders(EncoderSpecs, Settings) of 
                                        {ok, Encoders} -> 
                                            {ok, #{ settings => Settings,
                                                    init => Init,
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
    end;

compile(_, Settings) ->
    {ok, #{ settings => Settings,
            init => #{},
            update => #{},
            decoders => #{},
            encoders => #{},
            effects => #{}}}.

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

compile_effect_decoders(Effects, Ctx) ->
    compile_effect_decoders(maps:keys(Effects), Effects, Ctx, #{}).

compile_effect_decoders([], _, _, Out) -> {ok, Out};
compile_effect_decoders([Eff|Rem], Effects, Ctx, Out) ->
    case compile_decoders(maps:get(Eff, Effects), Ctx, #{}) of 
        {ok, Decs} ->
            compile_effect_decoders(Rem, Effects, Ctx, Out#{ Eff => Decs });
        Other ->
            Other
    end.

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
    case cmencode:encode(#{ type => object,
                            spec => EffSettingsSpec }, #{}, Settings) of 
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


compile_update_model(#{ model := ModelSpec}, Settings, Out) ->
    case term(ModelSpec, Settings) of
        {ok, Model} ->
            {ok, Out#{ model => Model }};
        Other ->
            Other
    end;

compile_update_model(_, _, Out) -> {ok, Out}.

compile_update_cmds(#{ cmds := CmdsSpec }, Settings, Out) ->
    case term(CmdsSpec, Settings) of 
        {ok, Cmds} ->
            {ok, Out#{ cmds => Cmds }};
        Other ->
            Other
    end;

compile_update_cmds(_, _, Out) -> {ok, Out}.


compile_update_condition(#{ condition := ConditionSpec }, Settings, Out) ->
    case term(ConditionSpec, Settings) of 
        {ok, Condition} ->
            {ok, Out#{ condition => Condition }};
        Other ->
            Other
    end;

compile_update_condition(_, _, Out) -> {ok, Out}.


compile_update_where(#{ where := WhereSpec }, Settings, Out) ->
    case term(WhereSpec, Settings) of 
        {ok, Where} ->
            {ok, Out#{ where => Where }};
        Other ->
            Other
    end;

compile_update_where(_, _, Out) -> {ok, Out}.

compile_update(Spec, Settings) -> 
    case compile_update_model(Spec, Settings, #{}) of 
        {ok, C1} ->
            case compile_update_cmds(Spec, Settings, C1) of 
                {ok, C2} ->
                    case compile_update_condition(Spec, Settings, C2) of 
                        {ok, C3} ->
                            compile_update_where(Spec, Settings, C3);
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end.



compile_object([], _, _, Out) -> {ok, Out};
compile_object([K|Rem], Spec, Settings, Out) -> 
    case term(maps:get(K, Spec), Settings) of 
        {ok, V} -> 
            compile_object(Rem, Spec, Settings, Out#{ K => V});
        Other -> 
            Other
    end.


terms(Specs, Settings) when is_map(Specs) ->
    terms(maps:keys(Specs), Specs, Settings, #{});


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

terms([], _, _, Out) -> {ok, Out};
terms([K|Rem], Specs, Settings, Out) ->
    case term(maps:get(K, Specs), Settings) of
        {ok, Compiled} ->
            terms(Rem, Specs, Settings, Out#{ K => Compiled}); 
        Other ->
            Other
    end.

term(V, _) when is_atom(V) or is_number(V) or is_binary(V) -> 
    {ok, V};

term(#{ type := expression, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ expression => Compiled}};
        Other -> 
            Other
    end;

term(#{ type := encoded, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ encoded => Compiled}};
        Other -> 
            Other
    end;

term(#{ type := list, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ list => Compiled}};
        Other -> 
            Other
    end;

term(#{ type := by_appending, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ by_appending => Compiled}};
        Other -> 
            Other
    end;

term(#{ type := by_replacing, 
        spec := Spec }, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            {ok, #{ by_replacing => Compiled }};
        Other -> 
            Other
    end;

term(#{ type := by_replacing, 
        items := ItemsSpec,
        with := WithSpec}, Settings) -> 
    case term(ItemsSpec, Settings) of 
        {ok, Items} ->
            case term(WithSpec, Settings) of 
                {ok, With} -> 
                    {ok, #{ by_replacing => #{ items => Items,
                                               with => With }}};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ type := by_removing, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ by_removing => Compiled}};
        Other -> 
            Other
    end;


term(#{ type := 'case',
        spec := Spec,
        'of' := Clauses,
        default := DefaultSpec }, Settings) when is_map(Clauses) ->

    case term(Spec, Settings) of
        {ok, CompiledSpec} ->
            case term(DefaultSpec, Settings) of 
                {ok, CompiledDefaultSpec} ->
                    case terms(Clauses, Settings) of 
                        {ok, CompiledClauses} ->
                            {ok, #{ 'case' => CompiledSpec,
                                    'of' => CompiledClauses,
                                    otherwise => CompiledDefaultSpec }};
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;



term(#{ type := split, spec := Spec, separator := Separator }, Settings) ->
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            case term(Separator, Settings) of 
                {ok, Sep} ->
                    {ok, #{ split => Compiled, using => Sep}};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;

term(#{ type := join, spec := Spec, separator := Separator }, Settings) ->
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            case term(Separator, Settings) of 
                {ok, Sep} ->
                    {ok, #{ join => Compiled, using => Sep}};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;


term(#{ type := head, spec := Spec }, Settings) ->
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            {ok, #{ head => Compiled}};
        Other ->
            Other
    end;

term(#{ type := first, spec := Spec }, Settings) ->
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            {ok, #{ first => Compiled}};
        Other ->
            Other
    end;

term(#{ type := last, spec := Spec }, Settings) ->
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            {ok, #{ last => Compiled}};
        Other ->
            Other
    end;

term(#{ type := tail, spec := Spec }, Settings) ->
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            {ok, #{ tail => Compiled}};
        Other ->
            Other
    end;

term(#{ type := size_of,
        spec := Spec }, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ size_of => Compiled }};
        Other -> 
            Other
    end;

term(#{ type := char, spec := CharSpec, in := InSpec }, Settings) ->
    case term(InSpec, Settings) of 
        {ok, In} ->
            case term(CharSpec, Settings) of 
                {ok, Char} ->
                    {ok, #{ char => Char, in => In}};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;

term(#{ type := list, size := Size}, _) -> 
    {ok, #{ list => #{ size => Size }}};

term(#{ type := object, spec := Spec}, Settings) -> 
    case compile_object(maps:keys(Spec), Spec, Settings, #{}) of 
        {ok, Compiled} -> 
            {ok, #{ object => Compiled}};
        Other -> 
            Other
    end;

term(#{ type := text, key := _, in := _}=Spec, Settings) ->
    case term(maps:without([type], Spec), Settings) of 
        {ok, Compiled} -> 
            {ok, #{ text => Compiled}};
        Other -> 
            Other
    end;


term(#{ type := text,
        spec := Spec }, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ text => Compiled}};
        Other -> 
            Other
    end;

term(#{ literal := V }, _) ->  {ok, V};

term(#{ type := true }, _) -> {ok, true};
term(#{ type := false}, _) -> {ok, false};


term(#{ type := boolean, spec := Spec}, _) when is_atom(Spec) ->
    {ok, Spec};

term(#{ type := boolean, spec := Spec}, Settings) ->
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            {ok, #{ boolean => Compiled}};
        Other ->
            Other
    end;

term(#{ type := boolean }, _) -> 
    {ok, #{ any => boolean }};

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

term(#{ type := lowercase,
        spec := Spec }, Settings) ->
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            {ok, #{ lowercase => Compiled }};
        Other ->
            Other
    end;

term(#{ type := uppercase,
        spec := Spec }, Settings) ->
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            {ok, #{ uppercase => Compiled }};
        Other ->
            Other
    end;

term(#{ type := format, 
        params := ParamsSpec,
        pattern := PatternSpec }, Settings) -> 
    case term(PatternSpec, Settings) of 
        {ok, Pattern} -> 
            case term(ParamsSpec, Settings) of 
                {ok, Params} -> 
                    {ok, #{ format_text => #{ pattern => Pattern,
                                              params => Params }}};
                Other -> 
                    Other
            end;
        Other ->
            Other
    end;

term(#{ type := format, 
        date := DateSpec,
        pattern := PatternSpec }, Settings) -> 
    case term(PatternSpec, Settings) of 
        {ok, Pattern} -> 
            case term(DateSpec, Settings) of 
                {ok, Date} -> 
                    {ok, #{ format_date => #{ pattern => Pattern,
                                              date => Date }}};
                Other -> 
                    Other
            end;
        Other ->
            Other
    end;

term(#{ type := divide,
        spec := Specs } = Spec, Settings) -> 
    case terms(Specs, Settings) of 
        {ok, CompiledTerms} ->
            Expr = #{ divide => CompiledTerms },
            case maps:get(decimals, Spec, undef) of 
                undef ->
                    {ok, Expr};
                Decs -> 
                    case term(Decs, Settings) of 
                        {ok, D} ->
                            {ok, Expr#{ decimals => D}};
                        Other ->
                            Other
                    end
            end;
        Other -> 
            Other
    end;


term(#{ type := sum,
        spec := Specs }, Settings) when is_list(Specs) -> 
    case terms(Specs, Settings) of 
        {ok, CompiledTerms} -> 
            {ok, #{ sum => CompiledTerms}};
        Other -> 
            Other
    end;

term(#{ type := sum,
        spec := Spec }, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} ->
            {ok, #{ sum => Compiled}};
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

term(#{ markdown := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ markdown => Compiled }};
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

term(#{ type := other_than, spec := Spec }, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ other_than => Compiled }};
        Other -> 
            Other
    end;

term(#{ type := greater_than, spec := Terms }, Settings) -> 
    case terms(Terms, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ greater_than => Compiled }};
        Other -> 
            Other
    end;

term(#{ type := lower_than, spec := Terms }, Settings) -> 
    case terms(Terms, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ lower_than => Compiled }};
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

term(#{ type := merge, spec := Specs }, Settings) ->
    case terms(Specs, Settings) of 
        {ok, Compiled} ->
            {ok, #{ merge => Compiled}};
        Other ->
            Other
    end;

term(#{ type := iterate,
        spec := #{ source := Source,
                   filter := Filter,
                   context := Context,
                   dest := Dest,
                   as := As }}, Settings) -> 
    case term(Source, Settings) of 
        {ok, S} ->
            case term(Filter, Settings) of 
                {ok, F} -> 
                    case term(Dest, Settings) of 
                        {ok, D} ->
                            case term(As, Settings) of 
                                {ok, A} ->
                                    case term(Context, Settings) of 
                                        {ok, C} ->
                                            {ok, #{ iterate => #{ source => S,
                                                                  context => C,
                                                                  dest => D,
                                                                  filter => F,
                                                                  as => A }}};
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
    case term(AttrsSpec, Settings) of 
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

term(#{ view := ViewSpec, params := ParamsSpec, condition := ConditionSpec}, Settings) -> 
    case term(ViewSpec, Settings) of
        {ok, View} -> 
            case term(ParamsSpec, Settings) of 
                {ok, Params} -> 
                    case term(ConditionSpec, Settings) of 
                        {ok, Condition} -> 
                            {ok, #{ name => View,
                                    condition => Condition,
                                    params => Params }};
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ view := _, condition := _} = Spec, Settings) -> 
    term(Spec#{ params => #{}}, Settings);

term(#{ view := _, params := _} = Spec, Settings) -> 
    term(Spec#{ condition => true }, Settings);

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


term(#{ type := i18n,
        spec := Spec } = Spec0, Settings) -> 
    case term(Spec, Settings) of 
        {ok, K} ->
            Compiled = #{ i18n => K },
            case maps:get(lang, Spec0, undef) of 
                undef ->
                    {ok, Compiled};
                Lang ->
                    case term(Lang, Settings) of 
                        {ok, L} ->
                            {ok, Compiled#{ lang => L }};
                        Other ->
                            Other
                    end
            end;
        Other ->
            Other
    end;

term(#{ key := KeySpec, in := InSpec } = Spec, Settings) ->
    case term(KeySpec, Settings) of 
        {ok, Key} ->
            case term(InSpec, Settings) of 
                {ok, In} ->
                    with_default(Spec, #{ key =>  Key,
                                          in => In}, Settings);
                Other -> 
                    Other
            end;
        Other ->
            Other
    end;

term(#{ key := KeySpec } = Spec, Settings) ->
    case term(KeySpec, Settings) of 
        {ok, Key} ->
            with_default(Spec, #{key => Key}, Settings);
        Other ->
            Other
    end;

term(object, _) -> 
    {ok, #{ type => object }};

term(#{ value := V}, _) -> {ok, V};

term(#{ type := number }, _) -> 
    {ok, #{ any => number }};

term(#{ type := text, value := V }, _) -> 
    {ok, #{ text => V }};

term(#{ type := text, spec := Spec }, Settings) ->
    case term(Spec, Settings) of 
        {ok, V} when is_binary(V) ->
            {ok, V};
        {ok, Spec2} ->
            {ok, #{ text => Spec2}};
        Other ->
            Other
    end;

term(#{ type := text }, _) -> 
    {ok, #{ any => text }};

term(#{ type := list}, _) -> 
    {ok, #{ any => list }};


term(#{ type := object}, _) -> 
    {ok, #{ any => object }};

term(#{ type := file }, _) -> 
    {ok, #{ any => file }};

term(#{ type := is_set, spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Expr} -> 
            {ok, #{ is_set => Expr }};
        Other -> 
            Other
    end;

term(#{ type := 'not', spec := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Expr} -> 
            {ok, #{ 'not' => Expr }};
        Other -> 
            Other
    end;

term(#{ type := 'and', spec := Spec}, Settings) -> 
    case terms(Spec, Settings) of 
        {ok, Exprs} -> 
            {ok, #{ 'and' => Exprs }};
        Other -> 
            Other
    end;

term(#{ type := 'or', spec := Spec}, Settings) -> 
    case terms(Spec, Settings) of 
        {ok, Exprs} -> 
            {ok, #{ 'or' => Exprs }};
        Other -> 
            Other
    end;





term(#{ encoder := Encoder,
        effect := Effect }, _) when is_binary(Encoder) andalso is_binary(Effect) -> 
    {ok, #{ encoder => Encoder,
            effect =>  Effect }};

term(#{ encoder := EncoderSpec,
        effect := EffectSpec }, Settings) ->
    case term(EncoderSpec, Settings) of 
        {ok, Enc} ->
            case term(EffectSpec, Settings) of 
                {ok, Eff} ->
                    {ok, #{ encoder => Enc,
                            effect => Eff }};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;

term(#{ effect := Effect }, _) when is_binary(Effect) ->
    {ok, #{ effect =>  Effect }};

term(#{ effect := EffectSpec }, Settings) ->
    case term(EffectSpec, Settings) of 
        {ok, Eff} ->
            {ok, #{ effect =>  Eff }};
        Other ->
            Other
    end;

term(#{ encoder := Encoder }, _) when is_binary(Encoder) -> 
    {ok, #{ encoder => Encoder }};

term(#{ encoder := EncoderSpec }, Settings) ->
    case term(EncoderSpec, Settings) of 
        {ok, Enc} ->
            {ok, #{ encoder => Enc }};
        Other ->
            Other
    end;


term(#{ type := encode,
        source := SourceSpec,
        dest := DestSpec,
        as := AsSpec }, Settings) ->
    case term(SourceSpec, Settings) of
        {ok, S} ->
            case term(DestSpec, Settings) of 
                {ok, D} ->
                    Compiled = #{ encode => S, with => D },
                    case AsSpec of 
                        none ->
                            {ok, Compiled};
                        _ -> 
                            case term(AsSpec, Settings) of 
                                {ok, As} ->
                                    {ok, Compiled#{ as => As}};
                                Other ->
                                    Other
                            end
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ timestamp := #{ format := FormatSpec,
                        value := Spec }}, Settings) -> 

    case term(FormatSpec, Settings) of 
        {ok, Format} -> 
            case term(Spec, Settings) of 
                {ok, Value} -> 
                    {ok, #{ timestamp => #{ format => Format,
                                            value => Value }}};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ with := Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ with => Compiled}};
        Other -> 
            Other
    end;

term(#{ without:= Spec}, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ without => Compiled}};
        Other -> 
            Other
    end;

term(#{ prettify := Spec }, Settings) -> 
    case term(Spec, Settings) of 
        {ok, Compiled} -> 
            {ok, #{ prettify => Compiled}};
        Other -> 
            Other
    end;

term(#{ code := #{ lang := LangSpec,
                   source := SourceSpec }}, Settings) -> 
    case term(LangSpec, Settings) of 
        {ok, Lang} -> 
            case term(SourceSpec, Settings) of 
                {ok, Source} -> 
                    {ok, #{ code => #{ lang => Lang,
                                       source => Source }}};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

term(#{ chart := #{ type := Type,
                    labels := Labels,
                    data := Data,
                    low := Low }}, Settings) ->
    case term(Type, Settings) of 
        {ok, T} ->
            case term(Labels, Settings) of 
                {ok, L} ->
                    case term(Data, Settings) of 
                        {ok, D} ->
                            case term(Low, Settings) of 
                                {ok, Lo} ->

                                    {ok, #{ chart => #{ type => T,
                                                        labels => L,
                                                        low => Lo,
                                                        data => D }}};
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

term(Map, _) when is_map(Map) andalso map_size(Map) =:= 0 ->
    {ok, #{}};

term(#{ type := keyword, 
        spec := #{ type := text, spec := Spec }}, _) when is_binary(Spec) ->
    {ok, #{ text => Spec}};

term(Other, Settings) ->
    cmkit:warning({cmelementary, default_as_object, Other}),
    term(#{ type => object,
            spec => Other }, Settings).

with_default(#{ default := DefaultSpec}, Compiled, Settings) ->
    case term(DefaultSpec, Settings) of 
        {ok, Default} ->
            {ok, Compiled#{ default => Default }};
        Other ->
            Other
    end;

with_default(_, Compiled, _) -> {ok, Compiled}.
