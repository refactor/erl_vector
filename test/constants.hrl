-define(EPISILON,  0.0002175).

-define(EQUAL_LIST(L1, L2),
    lists:all(fun({E,R}) ->
                      Diff = abs(E - R),
                      ?debugFmt("diff: ~p", [Diff]),
                      Diff =< ?EPISILON
              end,
              lists:zip(L1, L2))).
