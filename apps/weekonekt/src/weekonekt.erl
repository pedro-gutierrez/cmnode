-module(weekonekt).
-export([import_image/2, import/0]).

import(Name) ->
    {ok, Data} = file:read_file(cmkit:asset(Name)),
    weekonekt_wp_importer:import(Data).

import() -> import("wordpress.xml").

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
