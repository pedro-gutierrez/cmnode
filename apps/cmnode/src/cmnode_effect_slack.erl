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
                     enabled := Enabled } = Slack }}} -> 

            Subject = <<"New docker image ready">>,
            Body = <<"Image ", Name/binary, ":", Tag/binary, " is now ready">>,

            case Enabled of 
                true -> 
                    cmslack:success(Slack#{ subject => Subject,
                                            text => Body
                                          });
                false -> 
                    cmkit:log({slack, Settings, disabled, Subject, Body})
            end;
        
        Other -> 
            cmkit:warning({effect, slack, invalid_settings, Settings, Other})
    end.
