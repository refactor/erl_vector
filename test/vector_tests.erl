-module(vector_tests).
-include_lib("eunit/include/eunit.hrl").

-include("constants.hrl").

sigmoid_test() ->
    INFINITY = <<0, 0, 128, 127>>,
    B1 = <<0.1:32/float-native, 0.2:32/float-native, INFINITY/binary>>,
    V1 = vector:build_vector(B1),
    V2 = vector:sigmoid(V1),
    L2 = [F || <<F:32/float-native>> <= vector:get_vectorbin(V2)],
    Expected = [0.52498,0.54983,0.9999],
    ?assert(?EQUAL_LIST(Expected, L2)).

add_test() ->
    B1 = <<0.1:32/float-native, 1.2:32/float-native, 2.3:32/float-native>>,
    V1 = vector:build_vector(B1),
    B2 = <<3.2:32/float-native, 2.1:32/float-native, 1.0:32/float-native>>,
    V2 = vector:build_vector(B2),
    V = vector:add(V1, V2),
    B = binary:copy(<<3.3:32/float-native>>, 3),
    ?assertEqual(B, vector:get_vectorbin(V)).

digit_to_vector_test() ->
    V = vector:digit_to_vector(3),
    B = <<0.0:32/float-native, 0.0:32/float-native, 0.0:32/float-native, 1.0:32/float-native, 0.0:32/float-native, 0.0:32/float-native, 0.0:32/float-native, 0.0:32/float-native, 0.0:32/float-native, 0.0:32/float-native>>,
    ?assertEqual(B, vector:get_vectorbin(V)).

add_scalev_test() ->
    B1 = <<0.1:32/float-native, 1.2:32/float-native, 2.3:32/float-native>>,
    V1 = vector:build_vector(B1),
    B2 = <<0.1:32/float-native, 1.1:32/float-native, 11.1:32/float-native>>,
    V2 = vector:build_vector(B2),
    V = vector:add_scalev(V1, V2, 2.0),
    B = <<0.3:32/float-native, 3.4:32/float-native, 24.5:32/float-native>>,
    ?assertEqual(B, vector:get_vectorbin(V)).

schur_prod_test() ->
    B1 = <<0.1:32/float-native, 1.2:32/float-native, 2.3:32/float-native>>,
    V1 = vector:build_vector(B1),
    B2 = <<3.0:32/float-native, 2.0:32/float-native, 1.0:32/float-native>>,
    V2 = vector:build_vector(B2),

    V = vector:schur_prod(V1, V2),
    B = <<0.3:32/float-native, 2.4:32/float-native, 2.3:32/float-native>>,
    ?assertEqual(B, vector:get_vectorbin(V)).

v_multiply_v_test() ->
    B1 = <<0.1:32/float-native, 1.0:32/float-native, 2.0:32/float-native>>,
    V1 = vector:build_vector(B1),
    B2 = <<3.0:32/float-native, 2.0:32/float-native, 1.0:32/float-native>>,
    V2 = vector:build_vector(B2),

    M = vector:v_multiply_v(V1, V2),
    ?assertEqual(3, length(M)),
    MV1 = lists:nth(1, M),
    MV2 = lists:nth(2, M),
    MV3 = lists:nth(3, M),
    ?assertEqual(<<0.3:32/float-native, 0.2:32/float-native, 0.1:32/float-native>>,
                 vector:get_vectorbin(MV1)),
    ?assertEqual(<<3.0:32/float-native, 2.0:32/float-native, 1.0:32/float-native>>,
                 vector:get_vectorbin(MV2)),
    ?assertEqual(<<6.0:32/float-native, 4.0:32/float-native, 2.0:32/float-native>>,
                 vector:get_vectorbin(MV3)).

transpose_test() ->
    B1 = <<0.1:32/float-native, 1.0:32/float-native, 2.0:32/float-native>>,
    V1 = vector:build_vector(B1),
    B2 = <<3.0:32/float-native, 2.0:32/float-native, 1.0:32/float-native>>,
    V2 = vector:build_vector(B2),

    TM = vector:transpose([V1,V2]),
    ?assertEqual(<<0.1:32/float-native, 3.0:32/float-native>>, 
                 vector:get_vectorbin(lists:nth(1, TM))),
    ?assertEqual(<<1.0:32/float-native, 2.0:32/float-native>>, 
                 vector:get_vectorbin(lists:nth(2, TM))),
    ?assertEqual(<<2.0:32/float-native, 1.0:32/float-native>>, 
                 vector:get_vectorbin(lists:nth(3, TM))),
    ?assertEqual(3, length(TM)).

transpose2_test() ->
    B1 = <<0.1:32/float-native, 1.0:32/float-native, 2.0:32/float-native, 3.0:32/float-native, 4.0:32/float-native, 5.0:32/float-native>>,
    V1 = vector:build_vector(B1),
    B2 = <<5.0:32/float-native, 4.0:32/float-native, 3.0:32/float-native, 2.0:32/float-native, 1.0:32/float-native, 0.0:32/float-native>>,
    V2 = vector:build_vector(B2),
    B3 = <<5.3:32/float-native, 4.3:32/float-native, 3.3:32/float-native, 2.3:32/float-native, 1.3:32/float-native, 0.3:32/float-native>>,
    V3 = vector:build_vector(B3),
    B4 = <<5.4:32/float-native, 4.4:32/float-native, 3.4:32/float-native, 2.4:32/float-native, 1.4:32/float-native, 0.4:32/float-native>>,
    V4 = vector:build_vector(B4),
    B5 = <<5.5:32/float-native, 4.5:32/float-native, 3.5:32/float-native, 2.5:32/float-native, 1.5:32/float-native, 0.5:32/float-native>>,
    V5 = vector:build_vector(B5),

    TM = vector:transpose([V1,V2,V3,V4,V5]),
    ?assertEqual(<<0.1:32/float-native, 5.0:32/float-native,5.3:32/float-native,5.4:32/float-native, 5.5:32/float-native>>, 
                 vector:get_vectorbin(lists:nth(1, TM))),
    ?assertEqual(<<1.0:32/float-native, 4.0:32/float-native, 4.3:32/float-native, 4.4:32/float-native, 4.5:32/float-native>>, 
                 vector:get_vectorbin(lists:nth(2, TM))),
    ?assertEqual(<<5.0:32/float-native, 0.0:32/float-native, 0.3:32/float-native, 0.4:32/float-native, 0.5:32/float-native>>, 
                 vector:get_vectorbin(lists:nth(6, TM))),
    ?assertEqual(6, length(TM)).

