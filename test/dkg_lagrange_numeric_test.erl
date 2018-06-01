-module(dkg_lagrange_numeric_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Indices = lists:seq(1, 3),
    io:format("Indices: ~p~n", [Indices]),
    Alpha = 10,
    io:format("Alpha: ~p~n", [Alpha]),
    Coefficients = coefficients(Indices, Alpha),
    io:format("Coefficients: ~p~n", [Coefficients]),
    %% TODO: apply coefficients and shares?
    ok.

coefficients(Indices, Alpha) ->
    One = 1,
    lists:reverse(lists:foldl(fun({I, Index}, Acc1) ->
                                      Num = lists:foldl(fun(E, Acc) ->
                                                                Acc * E
                                                        end, One, [ Idx - Alpha || {J, Idx} <- enumerate(Indices), J /= I]),

                                      Den = lists:foldl(fun(E, Acc) ->
                                                                Acc * E
                                                        end, One, [ Idx - Index  || {J, Idx} <- enumerate(Indices), J /= I]),
                                      [Num/Den | Acc1]
                              end, [], enumerate(Indices))).

%% apply(Coefficients, Shares) ->
%%     Zero = 0,
%%     lists:foldl(fun({Coefficient, Share}, Acc) ->
%%                         Acc + (Share * Coefficient)
%%                 end, Zero, lists:zip(Coefficients, Shares)).

enumerate(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).
