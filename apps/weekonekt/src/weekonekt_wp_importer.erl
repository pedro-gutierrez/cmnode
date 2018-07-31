-module(weekonekt_wp_importer).
-export([import/1, handle/2]).

import(Data) ->
    DefaultAuthorEmail = <<"marcos@mail.com">>,
    DefaultAuthorId = case cmdb:get(users, {email, DefaultAuthorEmail}) of
                        {ok, [Id]} -> Id;
                        _ -> undef
                    end,
        
    Stats = #{ default_author => DefaultAuthorId,
                images => 0,
                reviews => 0 },
                                                              
    Res = cmxml:parse(Data, fun weekonekt_wp_parser:event/3, #{ state => none,
                                                                callback => {?MODULE, handle},
                                                                stats => Stats}),

    case Res of 
        {ok, {ok, Stats}, _} -> {ok, Stats};
        Other -> Other
    end.

handle(#{ id := Id, 
         url := Url, 
         title := Title, 
         date := Date,
         type := "attachment" }, #{ images := Images }=Stats) ->
    
    BinId = cmkit:to_bin(Id),
    BinUrl = cmkit:to_bin(Url),
    PKey = {image, BinId},
    Item = #{ id => BinId,
              url => BinUrl,
              title => Title,
              date => cmkit:to_millis(Date) },
    
    weekonekt:import_image(BinId, Url),
    cmdb:put_new(weekonekt, [{PKey, Item}]),
    Stats#{ images => Images + 1 };

handle(#{ id := Id, 
          content := #{ options := Opts,
                        images := ImageIds },
          geo := #{ lat := Lat, lon := Lon },
          title := Title, 
          date := Date,
          type := "page" }, #{ default_author := UserId,
                               reviews := Reviews }=Stats)  ->
        
    case ignored_entry(Id) of 
        false -> 

            CoverImage = case ImageIds of 
                             [] -> none;
                             [First|_] -> First
                         end,

            BinId = cmkit:to_bin(Id),
            PKey = {review, BinId},
            Item = #{ id => BinId,
                      title => Title,
                      lat => Lat,
                      lon => Lon,
                      date => cmkit:to_millis(Date),
                      options => Opts,
                      cover => CoverImage,
                      author => UserId,
                      images => ImageIds },

            cmdb:put(weekonekt, [{{reviews, UserId}, BinId},
                                     {PKey, Item}]),
            Stats#{ reviews => Reviews + 1 };
        true -> 
            cmkit:warning({weekonekt, skipped, Id, Title}),
            Stats
    end;
    
handle(#{ type := "nav_menu_item" }, Stats) -> Stats;
handle(#{ type := "feedback" }, Stats) -> Stats;

handle(Item, Stats) -> 
    cmkit:warning({weekonekt, unsupported, Item}),
    Stats.

ignored_entries() -> ["3", "2", "125", "519", "938", "947", "975" ].
ignored_entry(Id) -> lists:member(Id, ignored_entries()).

