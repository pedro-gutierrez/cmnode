-module(cmevents).
-export([reset/1]).
-export([push/5]).
-export([pull/2, pull/3, pull/4]).

% @doc Push a new event of the given name and
% data, for the given type of aggregate
% and instance specified by its id
push(_Bucket, _Type, _Id, _Name, _Data) -> 
    ok.

% @doc Pull events for the given type of aggregate
pull(_Bucket, _Type) -> [].

% @doc Pull events of the aggregate, specified
% by its type and id
pull(_Bucket, _Type, _Id) -> [].

% @doc Pull events of the given name, for 
% the aggregate specified by its type and id
pull(_Bucket, _Type, _Id, _Name) -> [].

% @doc Delete all events for the given type
% of aggregate. Use this with caution.
reset(Bucket) -> 
    cmevents_util:reset(Bucket).
