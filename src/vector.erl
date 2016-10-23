-module(vector).

-include("vector.hrl").

-export([multiply/2]).
-export([dot_prod/2]).
-export([schur_prod/2]).
-export([v_multiply_v/2]).
-export([add_scalev/3]).
-export([add/2]).
-export([sumlist/1]).
-export([transpose/1]).

-export([cost_derivative/2]).
-export([sigmoid/1]).
-export([sigmoid_prime/1]).
-export([argmax/1]).

-export([shuffle_divide/2]).
-export([randn/2]).

-export([digit_to_vector/1]).
-export([build_vector/1]).
-export([get_vectorbin/1]).
-export([mexp_vector/1]).

-on_load(on_load/0).
on_load() ->
	PrivDir = case code:priv_dir(?MODULE) of
		{error, _} ->
			AppPath = filename:dirname(filename:dirname("erl_vector")),
			%AppPath = filename:dirname(filename:dirname(code:which(?MODULE))),
			filename:join(AppPath, "priv");
		Path ->
			Path
	end,
	erlang:load_nif(filename:join(PrivDir, "erl_vector"), 0).
	%erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).

-spec randn(Row::pos_integer(), Column::pos_integer()) -> matrix().
randn(Row, Column) ->
    M = [ << <<(rand:normal()):32/float-native>> || _ <- lists:seq(1, Column)>> || _ <- lists:seq(1, Row)],
    [ build_vector(V) || V <- M ].
        
-spec shuffle_divide([vector()], pos_integer()) -> [[vector()]].
shuffle_divide([], _MiniBatchSize) ->
    [];
shuffle_divide(VectorList, MiniBatchSize) ->
    Len = length(VectorList),
    VectorMap = lists:foldl(fun(V, AccMap) ->
                                    Idx = maps:size(AccMap) + 1,
                                    AccMap#{Idx => V}
                            end,
                            #{},
                            VectorList),
    NewVM = shuffle_by(VectorMap, Len),
    MBs = lists:map(fun(N) ->
                      lists:map(fun(I) ->
                                        maps:get((N - 1) * MiniBatchSize + I, NewVM)
                                end,
                                lists:seq(1, MiniBatchSize))
              end,
              lists:seq(1, Len div MiniBatchSize)),
    Rest = Len rem MiniBatchSize,
    case Rest of
        0 -> 
            MBs;
        _ ->
            [lists:map(fun(I) -> maps:get(I, NewVM) end, lists:seq(Len - Rest + 1, Len))|MBs]
    end.

-spec shuffle_by(#{non_neg_integer() => vector()}, non_neg_integer()) -> #{non_neg_integer() => vector()}.
shuffle_by(M, 1) ->
    M;
shuffle_by(M, I) ->
    J = rand:uniform(I),
    VI = maps:get(I, M),
    VJ = maps:get(J, M),
    shuffle_by(M#{I := VJ, J := VI}, I - 1).

%% just for MNIST data: transform a digit to target vector from  output layer
-spec digit_to_vector(0..9) -> vector().
digit_to_vector(Dig) ->
    Bin = <<(binary:copy(<<0.0:32/float-native>>,Dig))/binary, 1.0:32/float-native, (binary:copy(<<0.0:32/float-native>>, 10 - Dig - 1))/binary>>,
    build_vector(Bin).

-spec build_vector(binary()) -> vector().
build_vector(_Bin) ->
	erlang:nif_error({not_loaded, ?MODULE}).

-spec transpose(matrix()) -> matrix().
transpose(_M) ->
	erlang:nif_error({not_loaded, ?MODULE}).

-spec multiply(matrix(), vector()) -> vector().
multiply(L, V) ->
    Res = << <<(dot_prod(X, V)):32/float-native>> || X <- L >>,
    build_vector(Res).

-spec dot_prod(vector(), vector()) -> float().
dot_prod(_V1, _V2) ->
	erlang:nif_error({not_loaded, ?MODULE}).

-spec v_multiply_v(vector(), vector()) -> matrix().
v_multiply_v(_V1, _V2) ->
	erlang:nif_error({not_loaded, ?MODULE}).

-spec schur_prod(vector(), vector()) -> vector().
schur_prod(_V1, _V2) ->
	erlang:nif_error({not_loaded, ?MODULE}).

-spec sumlist([vector()]) -> vector().
sumlist(_VList) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% V1 + V2 * Scale
-spec add_scalev(vector(), vector(), float()) -> vector().
add_scalev(_V1, _V2, _Scale) ->
	erlang:nif_error({not_loaded, ?MODULE}).

-spec add(vector(), vector()) -> vector().
add(_V1, _V2) ->
	erlang:nif_error({not_loaded, ?MODULE}).

cost_derivative(_A, _Y) ->
	erlang:nif_error({not_loaded, ?MODULE}).

sigmoid(_ResObj) ->
	erlang:nif_error({not_loaded, ?MODULE}).

sigmoid_prime(_ResObj) ->
	erlang:nif_error({not_loaded, ?MODULE}).

-spec argmax(vector()) -> non_neg_integer().
argmax(_ResObj) ->
	erlang:nif_error({not_loaded, ?MODULE}).

-spec get_vectorbin(vector()) -> binary().
get_vectorbin(_ResObj) ->
	erlang:nif_error({not_loaded, ?MODULE}).

-spec mexp_vector(vector()) -> vector().
mexp_vector(_ResObj) ->
	erlang:nif_error({not_loaded, ?MODULE}).
