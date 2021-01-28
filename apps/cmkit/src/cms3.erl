-module(cms3).
-export([put/1, get/1]).
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-define(APP, cms3).

config(#{ access_key := AccessKey,
          secret_key := SecretKey,
          host := Host,
          timeout := Timeout,
          port := Port, 
          scheme := Scheme,
          kind := Kind } = Spec) ->

    Cfg = erlcloud_s3:new(cmkit:to_list(AccessKey), 
                          cmkit:to_list(SecretKey), 
                          cmkit:to_list(Host),
                          Port),

    {BucketAccessMethod, BucketAfterHost} = bucket_access(Kind), 

    Cfg#aws_config{
      s3_region=region(Spec),
      s3_scheme=scheme(Scheme), 
      s3_bucket_access_method=BucketAccessMethod,
      s3_bucket_after_host=BucketAfterHost,
      timeout=Timeout};

config(#{ access_key := _,
          secret_key := _,
          host := _,
          timeout := _,
          port := _, 
          scheme := _} = Spec) ->

    config(Spec#{ access => auto });

config(#{ access_key := _,
          secret_key := _,
          host := _ ,
          timeout := _ } = Spec) ->


    config(Spec#{ port => 443, 
                  scheme => "https://" });

config(#{ access_key := _,
          secret_key := _ }=Spec) ->

    config(Spec#{ host => "s3-eu-west-3.amazonaws.com",
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

    Config = config(Spec),
    case ensure_bucket(Bucket, Config) of 
        ok ->
            cmkit:log({cms3, Spec}),
            s3_op(fun() -> 
                          case erlcloud_s3:put_object(cmkit:to_list(Bucket), 
                                                      cmkit:to_list(Key), 
                                                      cmkit:to_bin(Data), Config) of
                              [{version_id, _}|_] -> ok;
                              Error -> {error, Error}
                          end
                  end);
        Other ->
            {error, #{ error => bucket_error,
                       bucket => Bucket,
                       reason => Other}}
    end.

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

region(#{ region := R }) -> cmkit:to_list(R);
region(_) -> "us-east-1".


scheme(<<"http">>) -> "http://";
scheme(http) -> "http://";
scheme(<<"https">>) -> "https://";
scheme(https) -> "https://".

bucket_access(<<"minio">>) -> bucket_access(minio);
bucket_access(minio) -> {path, true};
bucket_access(_) -> {vhost, false}.

ensure_bucket(Bucket0, Config) ->
    Bucket = cmkit:to_list(Bucket0),
    case erlcloud_s3:check_bucket_access(Bucket, Config) of 
        ok ->
            ok;
        _ ->
            erlcloud_s3:create_bucket(Bucket, Config)
    end.
