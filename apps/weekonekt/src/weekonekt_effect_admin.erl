-module(weekonekt_effect_admin).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> weekonekt_admin.

effect_apply(#{ import := File,
                bucket := Bucket }, SessionId) ->

    
    Res = #{ status => importing },
    cmcore:update(SessionId, #{ wordpress => Res}),

    Res2 = case cmdb:get(Bucket, {file, File}) of 
        {ok, [#{ data := Bin }]} ->
            case weekonekt_wp_importer:import(Bin) of 
                {ok, #{ categories := Cats,
                        images := Images,
                        items :=  Items }} ->
                    
                    #{ status => finished,
                       categories => length(Cats),
                       images => length(Images),
                       items => length(Items) };

                Other -> 
                    cmkit:danger({weekonekt, import_error, File, Other}),
                    #{ status => error,
                       reason => Other }
            end;
        Other ->
            cmkit:danger({weekonekt, file_error, File, Other}),
            #{ status => error,
               reason => Other }
    end,
    cmcore:update(SessionId, #{ wordpress => Res2}).

