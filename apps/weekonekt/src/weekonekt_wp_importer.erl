-module(weekonekt_wp_importer).
-export([import/1, handle/1]).

import(Data) ->
    Res = cmxml:parse(Data, fun weekonekt_wp_parser:event/3, #{ state => none,
                                                          callback => {?MODULE, handle},
                                                          image => #{ url => none, title => none },
                                                          images => [],
                                                          items => [],
                                                          categories => []
                                                        }),
    case Res of 
        {ok, Summary, _} -> {ok, Summary};
        Other -> Other
    end.

handle(#{ id := Id, 
         url := Url, 
         title := Title, 
         date := Date,
         type := "attachment" }) ->
    
    BinId = cmkit:to_bin(Id),
    BinUrl = cmkit:to_bin(Url),
    PKey = {image, BinId},
    Item = #{ id => BinId,
              url => BinUrl,
              title => Title,
              date => Date },

    cmkit:log({weekonekt, image, PKey, Item}),
    cmdb:put_new(weekonekt, [{PKey, Item}]);

handle(Item) -> 
    cmkit:log({weekonekt_wp_importer, ignore, Item}). 


