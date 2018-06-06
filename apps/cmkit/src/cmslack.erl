-module(cmslack).
-export([danger/1, warn/1, success/1]).

danger(Spec) -> post(Spec, <<"danger">>).
warn(Spec) -> post(Spec, <<"warning">>).
success(Spec) -> post(Spec, <<"good">>).

post(#{ token := T, 
        subject := Subject,
        text := Text,
        channel := Ch }, Color) ->
    Msg = slacker_rich_messages:format(Subject, Text, Color),
    slacker_chat:post_message(T, Ch, <<>>, [{attachments, Msg}]).

