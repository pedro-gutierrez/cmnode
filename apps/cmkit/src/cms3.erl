-module(cms3).
-export([put/1, get/1]).
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-define(APP, cms3).

config(#{ access_key := AccessKey,
          secret_key := SecretKey,
          host := Host,
          timeout := Timeout }) ->

    Cfg = erlcloud_s3:new(cmkit:to_list(AccessKey), 
                          cmkit:to_list(SecretKey), 
                          cmkit:to_list(Host)),
    Cfg#aws_config{timeout=Timeout};

config(#{ access_key := _,
          secret_key := _ }=Config) ->
    config(Config#{ host => "s3-eu-west-3.amazonaws.com",
                    timeout => 60000 }).

s3_op(OpFun) -> 
    try OpFun() 
    catch
        _:{aws_error, {http_error, 404, _, _}} -> 
            {error, not_found};
        _:{aws_error, {http_error, 403, _, _}} -> 
            {error, forbidden};
        _:E -> 
            {error, E}
    end.

put(#{ bucket := Bucket,
       key := Key,
       data := Data}=Spec) -> 
   
    cmkit:log({cms3, Spec}),
    Config = config(Spec),
    s3_op(fun() -> 
                  case erlcloud_s3:put_object(cmkit:to_list(Bucket), 
                                              cmkit:to_list(Key), 
                                              cmkit:to_bin(Data), Config) of
                      [{version_id, _}|_] -> ok;
                      Error -> {error, Error}
                  end
          end).

get(#{ bucket := Bucket,
       key := Key }=Spec) ->

    Config = config(Spec),
    
    s3_op(fun() -> 
                  case erlcloud_s3:get_object(cmkit:to_list(Bucket),
                                              cmkit:to_list(Key), 
                                              Config) of
                      [_|_]=Meta ->
                          case proplists:get_value(content, Meta) of
                              undefined ->
                                  {error, no_content};
                              Data ->
                                  {ok, Data}
                          end;
                      Other ->
                          {error, Other}
                  end
          end).
