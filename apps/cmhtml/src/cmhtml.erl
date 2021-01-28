-module(cmhtml).
-export([compile/3]).

compile(V, Views, Settings) -> 
    case compiled_view(V, Views, Settings, Settings) of 
        {ok, Compiled} -> 
            {ok, render(Compiled)};
        Other -> 
            {error, Other}
    end.

encoded(Spec, Ctx, Settings) ->
    case cmencode:encode(Spec, maps:merge(Settings, Ctx)) of
        {ok, Encoded} -> {ok, Encoded};
        {error, E} -> 
            #{ error => encode,
               spec => Spec,
               reason => E }
    end.


compiled_view(#{ view := Name,
                 params := Params }, Views, Ctx, Settings) -> 

    case resolved(Name, Views, Ctx, Settings) of 
        {ok, Resolved} ->
            case encoded(Params, Ctx, Settings) of 
                {ok, EncodedParams} -> 
                    compiled_view(Resolved, Views, EncodedParams, Settings);
                Other -> 
                    #{ error => params,
                       view => Name,
                       reason => Other }
            end;
        Other -> 
            Other
    end;

compiled_view(#{ tag := Tag,
                 attrs := Attrs,
                 children := Children }, Views, Context, Settings) -> 
    case compiled_attrs(Attrs, Context, Settings) of 
        {ok, CompiledAttrs} ->
            case compiled_views(Children, Views, Context, Settings) of 
                {ok, CompiledChildren} ->
                    {ok, [Tag, CompiledAttrs, CompiledChildren]};
                Other ->
                    Other
            end;
        Other -> 
            #{ error => attrs,
               view => Tag,
               reason => Other }
    end;

compiled_view(#{ loop := _,
                 with := _,
                 context := _} = Spec, Views, Ctx, Settings) ->
    case compiled_views(Spec, Views, Ctx, Settings) of 
        {ok, Compiled} -> 
            {ok, list, Compiled};
        Other -> 
            #{ error => view,
               view => Spec,
               reason => Other }
    end;


compiled_view(#{ text := Spec}, _, Ctx, Settings) when is_map(Spec) -> 
    case encoded(Spec, Ctx, Settings) of 
        {ok, Encoded} -> 
            {ok, escapeHtml(Encoded)};
        Other -> 
            Other
    end;

compiled_view(#{ text := Bin}, _, _, _) when is_binary(Bin) ->
    {ok, escapeHtml(Bin)};

compiled_view(#{ text := Other}, _, _, _) ->
    {ok, escapeHtml(cmkit:to_bin(Other))}.


resolved(Name, Views, _, _) when is_atom(Name) or is_binary(Name) -> 
    case cmkit:value_at(Name, Views) of 
        undef -> 
            #{ view => Name,
               error => no_such_view};
        V -> 
            {ok, V}
    end;

resolved(Spec, Views, Ctx, Settings) when is_map(Spec) -> 
    case encoded(Spec, Ctx, Settings) of 
        {ok, Name} -> 
            resolved(Name, Views, Ctx, Settings);
        Other -> 
            Other
    end.

escapeHtml(Bin) ->
    binary:replace(binary:replace(Bin, <<"<">>, <<"&lt;">>, [global]), <<">">>, <<"&gt;">>, [global]).



generated(Items, View, Views, Ctx, Settings) -> 
    generated(Items, View, Views, Ctx, Settings, []).

generated([], _, _, _, _, Out) -> {ok, lists:reverse(Out)};
generated([I|Rem], View, Views, Ctx, Settings, Out) -> 
    case compiled_view(View, Views, #{ item => I,
                                       context => Ctx }, Settings) of 
        {ok, Compiled} -> 
            generated(Rem, View, Views, Ctx, Settings, [Compiled|Out]);
        Other -> 
            Other
    end.

compiled_views(#{ loop := ItemsSpec,
                  with := View,
                  context := ContextSpec }, Views, Ctx, Settings) -> 
    case resolved(View, Views, Ctx, Settings) of
        {ok, Resolved} ->
            case encoded(ContextSpec, Ctx, Settings) of 
                {ok, SharedCtx} -> 
                    case encoded(ItemsSpec, Ctx, Settings) of 
                        {ok, Items} -> 
                            generated(Items, Resolved, Views, maps:merge(Settings, SharedCtx), Settings);
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

compiled_views(List, Views, Ctx, Settings) when is_list(List) -> 
    compiled_views(List, Views, Ctx, Settings, []).

compiled_views([], _, _, _, Out) -> {ok, lists:reverse(Out)};
compiled_views([V|Rem], Views, Ctx, Settings, Out) -> 
    case compiled_view(V, Views, Ctx, Settings) of 
        {ok, list, Compiled} -> 
            compiled_views(Rem, Views, Ctx, Settings, lists:reverse(Compiled) ++ Out);
        {ok, Compiled} -> 
            compiled_views(Rem, Views, Ctx, Settings, [Compiled|Out]);
        Other -> 
            Other
    end.

compiled_attrs(Attrs, Ctx, Settings) -> 
    compiled_attrs(maps:keys(Attrs), Attrs, Ctx, Settings, #{}).

compiled_attrs([], _, _, _, Out) -> {ok, Out};
compiled_attrs([K|Rem], Attrs, Ctx, Settings, Out) -> 
    case encoded(maps:get(K, Attrs), Ctx, Settings) of 
        {ok, Encoded} -> 
            KBin = cmkit:to_bin(K),
            compiled_attrs(Rem, Attrs, Ctx, Settings, Out#{ KBin => Encoded});
        Other -> 
            Other
    end.

render([Tag, Attrs, Children]) when map_size(Attrs) =:= 0 -> 
    RenderedChildren = cmkit:bin_join(lists:map(fun render/1, Children)),
    <<"<", Tag/binary, ">",
      RenderedChildren/binary,
      "</", Tag/binary, ">" >>;

render([]) -> <<>>;

render([Tag, Attrs, Children]) -> 
    RenderedAttrs = render(Attrs),
    RenderedChildren = cmkit:bin_join(lists:map(fun render/1, Children)),
    <<"<", Tag/binary, RenderedAttrs/binary, ">",
      RenderedChildren/binary,
      "</", Tag/binary, ">" >>;


render(Attrs) when is_map(Attrs) -> 
    cmkit:bin_join(maps:fold(fun(K, V, Out) -> 
                                     [<<" ",  K/binary, "=\"", V/binary, "\"">>|Out]
                             end, [], Attrs));


render(Txt) when is_binary(Txt) -> 
    Txt;

render(Other) -> 
    cmkit:warning({cmhtml, render, unknown, Other}),
    cmkit:to_bin(Other).

