-module(cmcss).
-export([compile/4]).

compile(Colors, Fonts, Selectors, Settings) -> 
    case merge_colors(Colors, Settings) of 
        {ok, C} ->
            case merge_fonts(Fonts, Settings) of 
                {ok, F} ->
                    case encode_selectors(Selectors, C, F, Settings) of 
                        {ok, S} -> 
                            compile_selectors(S);
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end.

merge_colors(Colors, Settings) -> 
    case cmencode:encode_all(Colors, Settings) of
        {ok, C} ->
            {ok, lists:foldr(fun(C0, Merged) -> 
                            maps:merge(Merged, C0)
                        end, #{}, C)};
        Other -> 
            Other
    end.

merge_fonts(Fonts, Settings) -> 
    case cmencode:encode_all(Fonts, Settings) of
        {ok, F} ->
            {ok, lists:foldr(fun(F0, Merged) -> 
                            maps:merge(Merged, F0)
                        end, #{}, F)};
        Other -> 
            Other
    end.

encode_selectors(Selectors, C, F, Settings) -> 
    case cmencode:encode_all(Selectors, #{ colors => C,
                                           fonts => F,
                                           settings => Settings }) of 
        {ok, S} -> 
            {ok, lists:flatten(S)};
        Other -> 
            Other
    end.

compile_selectors(Selectors) ->
    compile_selectors(Selectors, <<>>, []).

compile_selectors([], _, Out) -> 
    {ok, cmkit:bin_join(lists:flatten(lists:reverse(Out)), <<>>)};

compile_selectors([S|Rem], P, Out) -> 
    case compile_selector(S, P) of 
        {ok, Rules} -> 
            compile_selectors(Rem, P, [Rules|Out]);
        Other -> 
            Other
    end.

compile_selector(S, P) -> 
    Sel = render_selector(S, P),
    case render_style(S, Sel) of 
        {ok, Style} -> 
            case compile_selectors(maps:get(selectors, S, []), Sel, []) of 
                {ok, Sels} -> 
                    {ok, [Style, Sels]};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end.

render_selector(#{ elem := N }, <<>>) ->
    <<N/binary>>;

render_selector(#{ elem := N }, P) ->
    <<P/binary, " ", N/binary>>;

render_selector(#{ class := N }, P) ->
    <<P/binary, ".", N/binary>>;

render_selector(#{ 'any-class' := N }, P) ->
    cmkit:bin_join(lists:map(fun(C) -> 
                        <<P/binary, ".", C/binary>> 
                        end, cmkit:bin_split(N, <<" ">>)), <<", ">>);

render_selector(#{ 'all-classes' := N }, <<>>) ->
    cmkit:bin_join(lists:map(fun(C) -> 
                                     <<".", C/binary>> 
                             end, cmkit:bin_split(N, <<" ">>)), <<>>);

render_selector(#{ 'all-classes' := N }, P) ->
    N2 = cmkit:bin_join(lists:map(fun(C) -> 
                                          <<".", C/binary>> 
                                  end, cmkit:bin_split(N, <<" ">>)), <<>>),
    <<P/binary, N2/binary>>;

render_selector(#{ 's-class' := N }, <<>>) ->
    <<".", N/binary>>;

render_selector(#{ 's-class' := N }, P) ->
    <<P/binary, " .", N/binary>>;

render_selector(#{ 'p-class' := N }, P) ->
    <<P/binary, ":", N/binary>>;

render_selector(#{ 'p-elem' := N }, P) ->
    <<P/binary, "::", N/binary>>;

render_selector(_, P) ->
    <<P/binary, " unknown-selector">>.

render_style(#{ style := Style }, Sel) -> 
    S = render(Style),
    {ok, <<Sel/binary, " {", S/binary, "}\n">> };

render_style(_, _) -> {ok, <<>>}. 

render(Props) when is_map(Props) -> 
    cmkit:bin_join(maps:fold(fun(K, V, Out) -> 
                                    Kbin = cmkit:to_bin(K),
                                    Vbin = cmkit:to_bin(V),
                                     [<<Kbin/binary, ":", Vbin/binary, ";">>|Out]
              end, [], Props), <<>>).
