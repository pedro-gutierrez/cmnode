-module(cmhtml).
-export([compile/3]).

compile(V, Views, Settings) -> 
    case compiled_view(V, Views,Settings) of 
        {ok, Compiled} -> 
            {ok, render(Compiled)};
        Other -> 
            {error, Other}
    end.

encoded(Spec, Ctx) ->
    case cmencode:encode(Spec, Ctx) of
        {ok, Encoded} -> {ok, Encoded};
        {error, E} -> 
            #{ error => encode,
               spec => Spec,
               reason => E }
    end.


compiled_view(#{ view := NameSpec,
                 params := Params }, Views, Context) -> 

    case encoded(NameSpec, Context) of 
        {ok, Name} -> 
            case resolved(Name, Views) of 
                {ok, Resolved} ->
                    case encoded(Params, Context) of 
                        {ok, EncodedParams} -> 
                            compiled_view(Resolved, Views, maps:merge(Context, EncodedParams));
                        Other -> 
                            #{ error => params,
                               view => Name,
                               reason => Other }
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            #{ error => view_name,
               spec => NameSpec,
               reason => Other }
    end;


compiled_view(#{ tag := Tag,
                attrs := Attrs,
                children := Children }, Views, Context) -> 
    case compiled_attrs(Attrs, Context) of 
        {ok, CompiledAttrs} ->
            case compiled_views(Children, Views, Context, []) of 
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

compiled_view(#{ text := Spec}, _, Ctx) -> 
    encoded(Spec, Ctx).

resolved(Name, Views) -> 
    case cmkit:value_at(Name, Views) of 
        undef -> 
            #{ view => Name,
               error => no_such_view};
        V -> 
            {ok, V}
    end.


compiled_views([], _, _, Out) -> {ok, lists:reverse(Out)};
compiled_views([V|Rem], Views, Ctx, Out) -> 
    case compiled_view(V, Views, Ctx) of 
        {ok, Compiled} -> 
            compiled_views(Rem, Views, Ctx, [Compiled|Out]);
        Other -> 
            Other
    end.

compiled_attrs(Attrs, Ctx) -> 
    compiled_attrs(maps:keys(Attrs), Attrs, Ctx, #{}).

compiled_attrs([], _, _, Out) -> {ok, Out};
compiled_attrs([K|Rem], Attrs, Ctx, Out) -> 
    case encoded(maps:get(K, Attrs), Ctx) of 
        {ok, Encoded} -> 
            KBin = cmkit:to_bin(K),
            compiled_attrs(Rem, Attrs, Ctx, Out#{ KBin => Encoded});
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

