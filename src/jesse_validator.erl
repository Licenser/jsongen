-module(jesse_validator).

-export([ start_validator/0
        , validate/3]).


start_validator() ->
    case code:which(jesse_schema_validator) of
        non_existing ->
            io:format
              ("*** Error: the Jesse library is not accessible.~n"),
            throw(bad);
        _ ->
            ok
    end.

validate(Schema, Root, Body) ->
    JSON = jsx:decode(Body),
    State = jesse_state:new(Root, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]),
    State1 = jesse_state:set_current_schema(State, Schema),
    try
        ResState = jesse_schema_validator:validate_with_state(Schema, JSON, State1),
        ErrorList = jesse_state:get_error_list(ResState),
        case ErrorList of
            [] -> true;
            E  ->
                io:format("Error: ~p~n", [E]),
                false
        end
    catch
        E1:E2 ->
            io:format("Error: ~p:~p~n", [E1, E2]),
            false
    end.
