-module(base64url).

-export([encode/1]).
-export([decode/1]).

% See:
%   https://tools.ietf.org/html/rfc4648

%%====================================================================
%% encode
%%====================================================================

-spec encode(binary()) -> binary().

encode(Binary) when is_binary(Binary) ->
    encode(Binary, <<>>).

%%--------------------------------------------------------------------

encode(<<>>, Encoded) ->
    Encoded;
encode(<<A:6, B:2>>, Encoded) ->
    <<Encoded/binary, (enc(A)), (enc(B bsl 4))>>;
encode(<<A:6, B:6, C:4>>, Encoded) ->
    <<Encoded/binary, (enc(A)), (enc(B)), (enc(C bsl 2))>>;
encode(<<A:6, B:6, C:6, D:6, Binary/binary>>, Encoded) ->
    encode(Binary, <<Encoded/binary, (enc(A)), (enc(B)), (enc(C)), (enc(D))>>).

%%====================================================================
%% decode
%%====================================================================

-spec decode(binary()) -> {ok, binary()} | false.

decode(Base64) when is_binary(Base64) ->
    decode(Base64, <<>>);
decode(_) ->
    false.

%%--------------------------------------------------------------------

decode(<<>>, Binary) ->
    {ok, Binary};
decode(<<Char, Base64/binary>>, Binary) ->
    case dec(Char) of
        A when is_integer(A) ->
            decode(Base64, Binary, A);

        _ ->
            false
    end;
decode(_, _) ->
    false.

%%--------------------------------------------------------------------

decode(<<Char, Base64/binary>>, Binary, A) ->
    case dec(Char) of
        B when is_integer(B) ->
            decode(Base64, Binary, A, B);

        _ ->
            false
    end;
decode(_, _, _) ->
    false.

%%--------------------------------------------------------------------

decode(<<>>, Binary, A, B) when (B band 15) =:= 0 ->
    {ok, <<Binary/binary, A:6, (B bsr 4):2>>};
decode(<<Char, Base64/binary>>, Binary, A, B) ->
    case dec(Char) of
        C when is_integer(C) ->
            decode(Base64, Binary, A, B, C);

        _ ->
            false
    end;
decode(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------

decode(<<>>, Binary, A, B, C) when (C band 3) =:= 0 ->
    {ok, <<Binary/binary, A:6, B:6, (C bsr 2):4>>};
decode(<<Char, Base64/binary>>, Binary, A, B, C) ->
    case dec(Char) of
        D when is_integer(D) ->
            decode(Base64, <<Binary/binary, A:6, B:6, C:6, D:6>>);

        _ ->
            false
    end;
decode(_, _, _, _, _) ->
    false.

%%====================================================================
%% convesion tables
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

enc_dec_test() ->
    [
        ?assertMatch({_, N}, {enc(N), dec(enc(N))})
        ||
        N <- lists:seq(0, 63)
    ].

%%--------------------------------------------------------------------

bad_test() ->
    Bad = [ N || N <- lists:seq(0, 255), dec(N) =:= bad ],
    ?assertEqual(256 - 64, length(Bad)).

-endif.

%%--------------------------------------------------------------------

enc(X) ->
    element(X + 1, {
        $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
        $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
        $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
        $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
        $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $-, $_
    }).

%%--------------------------------------------------------------------

dec(0) ->
    bad;
dec(X) ->
    % 1-based lookup
    element(X, {
            bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad, %1-15
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad, %16-31
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad, 62,bad,bad, %32-47
         52, 53, 54, 55, 56, 57, 58, 59, 60, 61,bad,bad,bad,bad,bad,bad, %48-63
        bad,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
         15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,bad,bad,bad,bad, 63,
        bad, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
         41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad
    }).

