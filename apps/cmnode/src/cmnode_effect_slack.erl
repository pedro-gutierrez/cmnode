-module(cmnode_effect_slack).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> slack.
effect_apply(#{ announcement := image_pushed,
                settings := Settings,
                image := #{ 
                  name := Name,
                  tag := Tag
                 }}, _Id) ->
    
    case cmconfig:settings(Settings, true) of 
        {ok, #{ slack := 
                #{ tests := #{
                     enabled := true } = Slack }}} -> 
          
            cmslack:success(Slack#{ subject => <<"New docker image ready">>,
                                    text => <<"Image ", Name/binary, ":", Tag/binary, " is now ready">> 
                                  });
        
        Other -> 
            cmkit:warning({effect, slack, invalid_settings, Settings, Other})
    end.
