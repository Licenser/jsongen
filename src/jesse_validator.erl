-module(jesse_validator).

-export([ start_validator/0
        , validate/2]).


start_validator() ->
    case code:which(jesse_schema_validator) of
        non_existing ->
            io:format
              ("*** Error: the Jesse library is not accessible.~n"),
            throw(bad);
        _ ->
            ok
    end.

validate(Schema, Body) ->
    JSON = mochijson2:decode(Body),
    case jesse:validate_with_schema(Schema, JSON, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]) of
        {ok, R} ->
            true;
        {error, E} ->
            io:format("Error: ~p~n", [E]),
            false;
        _ ->
            false
    end.
