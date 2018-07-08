-module(weekonekt).
-export([import_image/2]).

import_image(Id, Url) ->
    Job = #{ id => Id, 
             timestamp => cmkit:now(), 
             info => #{}, 
             spec => #{ 
               start => { cmtask, schedule, [
                                             thumbnails_fs,
                                             #{ settings => weekonekt, 
                                                url => Url,
                                                id => Id }]}, 
               stop => { cmtask, stop, Id }}},

    cmqueue:schedule(thumbnails_queue, Job).
