-module(cmnode_effect_slack).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> slack.
effect_apply(#{ settings := Settings,
                subject := Subject,
                text := Text }, _) -> 
    
    case cmconfig:settings(Settings, true) of 
        {ok, #{ slack := 
                #{ tests := #{
                     enabled := Enabled } = Slack }}} -> 

            case Enabled of 
                true -> 
                    cmslack:success(Slack#{ subject => Subject,
                                            text => Text
                                          });
                false -> 
                    cmkit:log({slack, Settings, disabled, Subject, Text})
            end;

        Other -> 
            cmkit:warning({effect, slack, invalid_settings, Settings, Other})
    end.
