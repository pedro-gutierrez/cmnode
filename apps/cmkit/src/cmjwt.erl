-module(cmjwt).
-export([sign/3, verify/2]).
-define(JWS, #{ <<"alg">> => <<"HS256">> }). 

sign(P, Ttl, K) when is_map(P) ->
    JWK = jwk(K),
    JWT = P#{ <<"exp">> => cmkit:millis() + Ttl },
    case jose_jws:compact(jose_jwt:sign(JWK, ?JWS, JWT)) of 
        {_, Signed} ->
            Signed;
        _ ->
            error
    end.

verify(B, K) ->
    JWK = jwk(K),
    case safe_verify(JWK, B) of
        {true, {jose_jwt, #{ <<"exp">> := Ttl }=JWT}, _} ->
            case Ttl > cmkit:millis() of 
                true ->
                    JWT;
                false ->
                    expired
            end;
        {true, {jose_jwt, JWT}, _} ->
            {ok, JWT};
        _ ->
            false
    end.

safe_verify(JWK, B) ->
    try jose_jwt:verify(JWK, B)
    catch
        _:_ ->
            false
    end.

jwk(K) -> 
    #{ <<"kty">> => <<"oct">>,
       <<"k">> => base64:encode(K) }.
