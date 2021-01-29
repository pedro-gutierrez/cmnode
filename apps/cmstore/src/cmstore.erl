-module(cmstore).
-export([reset/1,
         write/2,
         read/2]).

write(Bucket, Spec) -> 
    cmstore_util:write(Bucket, Spec).

read(Bucket, Spec) -> 
    cmstore_util:read(Bucket, Spec).

reset(Bucket) -> 
    cmstore_util:reset(Bucket).
