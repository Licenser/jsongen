%% Copyright (c) 2013, Ángel Herranz, Lars-Ake Fredlund, Sergio Gil
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc This module translates a JSON Schema into
%% an Erlang QuickCheck generator.
%% @author Ángel Herranz (aherranz@fi.upm.es), Lars-Ake Fredlund 
%% (lfredlund@fi.upm.es), Sergio Gil (sergio.gil.luque@gmail.com)
%% @copyright 2013 Ángel Herranz, Lars-Ake Fredlund, Sergio Gil


-module(jsongen).

-export([ json/1
        , json/2
        , version/0
        , binary_to_atom/1
        ]).

-compile([{nowarn_unused_function, [ insertType/2
                                   , number_positive/0
                                   , number_mul/1
                                   , selectSimpleType/0
                                   , randFlt/2
                                   , choose_from_list/3
                                   , pattern_gen/1
                                   , create_patterns/1
                                   ]}]).

-compile({no_auto_import, [floor/1]}).

%%LOGS
%%-define(debug,true).

-define(DEPTH, 10).

-ifdef(debug).
-define(LOG(X,Y),
        io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

-include_lib("eqc/include/eqc.hrl").

-type options() :: {root, jsg_jsonref:url()}.

%% @doc
%% Translates a JSON schema into an Erlang QuickCheck generator.
-spec json(jsg_json:json_term()) -> eqc_gen:gen(jsg_json:json_term()).
json(Schema) ->
    json(Schema, [{root,Schema}]).

-spec json(jsg_json:json_term(),options()) -> eqc_gen:gen(jsg_json:json_term()).
json(Schema, Options) ->
    json(Schema, Options, ?DEPTH).

-spec json(jsg_json:json_term(),options(), integer()) -> eqc_gen:gen(jsg_json:json_term()).
json(Schema, Options, Depth) when Depth =< 0 ->
    [];
json(Schema, Options, Depth) ->
    ?LOG("json(~s,~p)",[jsg_json:encode(Schema),Options]),
    case jsg_jsonschema:schemaType(Schema) of
        'ref' ->
            RootSchema = proplists:get_value(root,Options),
            RefSch = jsg_jsonref:unref(Schema,RootSchema),
            NewOptions =
                %% We should change the root but at this moment peano2 
                %% goes into an inifinite loop.
                %% TODO: do we need to maintain an environment!?
                %% AH: nop, maybe it is enough the root to be a URL instead of a schema
                %% [{root,RefSch}|proplists:delete(root,Options)],
                Options,
            ?LAZY(json(RefSch, NewOptions, Depth - 1));
        'type' ->
            gen_typed_schema(Schema, Options, Depth);
        'enum' ->
            eqc_gen:oneof(jsg_jsonschema:enumerated(Schema));
        'not' ->
            SingleSchema = jsg_jsonschema:notKeyword(Schema),
            case jsg_jsonschema:hasType(Schema) of
                true ->
                    ?SUCHTHAT(ValidationResult,
                              ?LET(Value, gen_typed_schema(Schema, Options, Depth),
                                   begin
                                       case valid_schemas(Value,[SingleSchema]) of
                                           0 -> Value;
                                           _ -> error
                                       end
                                   end),
                              ValidationResult =/= error);
                false ->
                    throw(not_keyword_with_no_schemas)
            end;
        'allOf' ->
            ListOfSchemas = jsg_jsonschema:allOf(Schema),
            case jsg_jsonschema:hasType(Schema) of
                true ->
                    ?SUCHTHAT(ValidationResult,
                              ?LET(Value, gen_typed_schema(Schema, Options, Depth),
                                   begin
                                       N = valid_schemas(Value,ListOfSchemas),
                                       if
                                           N == length(ListOfSchemas) -> Value;
                                           true -> error
                                       end
                                   end),
                              ValidationResult =/= error);
                false ->
                    ?SUCHTHAT(ValidationResult,
                              ?LET(I, eqc_gen:choose(1,length(ListOfSchemas)),
                                   ?LET(Value, json(lists:nth(I,ListOfSchemas),Options, Depth - 1),
                                        begin
                                            N = valid_schemas(Value,
                                                              delete_nth_element
                                                                (I,ListOfSchemas)),
                                            if
                                                N > (length(ListOfSchemas) -1) -> Value;
                                                true -> error
                                            end
                                        end)),
                              ValidationResult =/= error)
            end;

        'anyOf' ->
            ListOfSchemas = jsg_jsonschema:anyOf(Schema),
            ?LOG("AnyOf with List of Schemas: ~p~n",[ListOfSchemas]),
            case jsg_jsonschema:hasType(Schema) of
                true ->
                    ?SUCHTHAT(ValidationResult,
                              ?LET(Value, gen_typed_schema(Schema, Options, Depth),
                                   begin
                                       N = valid_schemas(Value,ListOfSchemas),
                                       if
                                           N > 0 -> Value;
                                           true -> error
                                       end
                                   end),
                              ValidationResult =/= error);
                false ->
                    ?SUCHTHAT(ValidationResult,
                              ?LET(I, eqc_gen:choose(1,length(ListOfSchemas)),
                                   ?LET(Value, json(lists:nth(I,ListOfSchemas),Options, Depth - 1),
                                        begin
                                            N = valid_schemas
                                                  (Value,
                                                   delete_nth_element(I,ListOfSchemas)),
                                            if
                                                N > 0 -> Value;
                                                true -> error
                                            end
                                        end)),
                              ValidationResult =/= error)
            end;

        'oneOf' ->
            ListOfSchemas = jsg_jsonschema:oneOf(Schema),
            case jsg_jsonschema:hasType(Schema) of
                true ->
                    ?SUCHTHAT(ValidationResult,
                              ?LET(Value, gen_typed_schema(Schema, Options, Depth),
                                   begin
                                       case valid_schemas(Value,ListOfSchemas) of
                                           1 -> Value;
                                           _ -> error
                                       end
                                   end),
                              ValidationResult =/= error);
                false ->
                    ?SUCHTHAT(ValidationResult,
                              ?LET(I, eqc_gen:choose(1,length(ListOfSchemas)),
                                   ?LET(Value, json(lists:nth(I,ListOfSchemas),Options, Depth - 1),
                                        begin
                                            case valid_schemas
                                                (Value,delete_nth_element(I,ListOfSchemas)) of
                                                0 -> Value;
                                                _ -> error
                                            end
                                        end)),
                              ValidationResult =/= error)
            end;

        'quickcheck' ->
            QcValue = jsg_jsonschema:propertyValue(Schema,"quickcheck"),
            case jsg_jsonschema:is_object(QcValue) of
                true ->
                    Name = binary_to_list(jsg_jsonschema:propertyValue(QcValue,"name")),
                    [Module,Fun] = re:split(Name,":"),
                    (binary_to_atom(Module)):
                        (binary_to_atom(Fun))(QcValue,jsg_store:get(eqc_gen_context));
                %% Name = binary_to_list(jsg_jsonschema:propertyValue(QcValue,"name")),
                %% [Module,Fun] = re:split(Name,":"),
                %% {struct, QcArgs} = QcValue,
                %%     (binary_to_atom(Module)):(binary_to_atom(Fun))(QcValue);
                %% (binary_to_atom(Module)):(binary_to_atom(Fun))(QcArgs);
                false ->
                    Name = binary_to_list(QcValue),
                    [Module,Fun] = re:split(Name,":"),
                    (binary_to_atom(Module)):
                        (binary_to_atom(Fun))(jsg_store:get(eqc_gen_context))
            end;

        _ -> 
            throw({bad_schema,Schema,?LINE})
    end.

binary_to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).

-spec valid_schemas(jsg_json:json_term(),[jsg_json:json_term()]) -> [jsg_json:json_term()].
valid_schemas(Value,Schemas) ->
    lists:foldl(fun (Schema,N) ->
                        case jsg_json_validate:validate(Value,Schema) of
                            true ->
                                Res = 1;
                            false ->
                                Res = 0;
                            maybe ->
                                Res = 1 %%for now...
                        end,
                        Res + N
                end, 0, Schemas).



-spec gen_typed_schema(jsg_json:json_term(), options(), integer()) -> jsg_json:json_term().
gen_typed_schema(Schema, Options, Depth) ->
    case jsg_jsonschema:type(Schema) of


        %% boolean
        %%     A JSON boolean. 
        <<"boolean">> ->
            boolean();


        %% integer
        %%     A JSON number without a fraction or exponent part. 
        <<"integer">> ->

            MaxScanned = jsg_jsonschema:keyword(Schema,"maximum"),
            ExcMaxScanned = jsg_jsonschema:keyword(Schema,"exclusiveMaximum",false),
            MinScanned = jsg_jsonschema:keyword(Schema,"minimum"),
            ExcMinScanned = jsg_jsonschema:keyword(Schema,"exclusiveMinimum",false),
            Multiple = jsg_jsonschema:keyword(Schema,"multipleOf",1),

                                                % Setting up keywords
            case MaxScanned of
                undefined ->
                    Max = undefined;
                _ ->
                    case ExcMaxScanned of
                        true -> Max = MaxScanned -1;
                        false -> Max = MaxScanned
                    end
            end,	    

            case MinScanned of
                undefined ->
                    Min = undefined;
                _ ->
                    case ExcMinScanned of
                        true -> Min = MinScanned +1;
                        false -> Min = MinScanned
                    end
            end,

            case {Min, Max} of
                {undefined, undefined} -> multiple_of(Multiple);
                {_Min, undefined} -> multiple_of_min(Multiple, Min);
                {undefined, _Max} -> multiple_of_max(Multiple, Max);
                {_Min, _Max} -> multiple_of_min_max(Multiple, Min,Max)
            end;

        %% Number
        %%     Any JSON number. Number includes integer.
        <<"number">> ->
            Max = jsg_jsonschema:keyword(Schema,"maximum"),
            ExcMax = jsg_jsonschema:keyword(Schema,"exclusiveMaximum",false),
            Min = jsg_jsonschema:keyword(Schema,"minimum"),
            ExcMin = jsg_jsonschema:keyword(Schema,"exclusiveMinimum",false),
            Mul = jsg_jsonschema:keyword(Schema,"multipleOf",1),


            case {Min, Max} of

                {undefined, undefined} ->
                    number();

                {Min, undefined} ->
                    number_mul_min(Mul,Min,ExcMin);

                {undefined, Max} ->
                    number_mul_max(Mul,Max, ExcMax);

                {Min,Max} ->
                    number_mul_min_max(Mul,Min,Max,{ExcMin,ExcMax})
            end;



        %% null
        %%     The JSON null value. 
        <<"null">> ->
            null();

        %% array
        %%     A JSON array. 
        <<"array">> ->
            MaxItems = jsg_jsonschema:keyword(Schema,"maxItems"),
            MinItems = jsg_jsonschema:keyword(Schema,"minItems",0),   
            AdditionalItems = jsg_jsonschema:additionalItems(Schema), 

            UniqueItems = jsg_jsonschema:keyword(Schema, "uniqueItems",false),
            ?LOG("AdditionalItems: ~p ~n",[AdditionalItems]),
            ?LOG("UniqueItems: ~p ~n",[UniqueItems]),
            ?LOG("Items: ~p ~n",[jsg_jsonschema:items(Schema)]),
            case jsg_jsonschema:items(Schema) of
                {itemSchema, ItemSchema} ->
                    case AdditionalItems of

                        true ->
                            arrayOfAny(MinItems,MaxItems,UniqueItems, Depth - 1);

                        false ->
                            array([ItemSchema], {MinItems,MaxItems}, UniqueItems, Depth - 1);

                        AdditionalSchema ->
                            ?LOG("AdditionalSchema is ~p~n",[AdditionalSchema]),
                            array([ItemSchema,AdditionalSchema], 
                                  {MinItems,MaxItems},UniqueItems, Depth - 1)

                    end;

                {empty,no_items} ->
                    arrayOfAny(MinItems,MaxItems,UniqueItems, Depth - 1);

                {error, bad_items_schema} ->
                    throw({bad_items_schema_in_array})
                                                %template(ItemsTemplate)
            end;


        %% object
        %%     A JSON object.
        <<"object">> ->
            Properties = jsg_jsonschema:properties(Schema),
            MinProperties = jsg_jsonschema:minProperties(Schema, 0),
            Required = jsg_jsonschema:keyword(Schema, "required",[]),
            PatternProperties = jsg_jsonschema:patternProperties(Schema),
            AdditionalProperties = jsg_jsonschema:additionalProperties(Schema),
            MaxProperties = jsg_jsonschema:maxProperties(Schema),

            case MaxProperties of
                undefined ->
                    MaxPropsGen = natural_gte(MinProperties);

                Value -> 
                    MaxPropsGen = Value
            end,                            

            ReqProps = [{P,S} || {P,S} <- Properties, lists:member(P, Required)],
            OptProps = [{P,S} || {P,S} <- Properties, not lists:member(P, Required)],       
            MinOpts = MinProperties - length(ReqProps),

            AddP = 
                case AdditionalProperties of 
                    false -> 
                        undefined;
                    true -> 
                        {};
                    AddSchema ->
                        AddSchema
                end,

            ?LOG("Max Properties: ~p~n",[MaxProperties]),
            ?LOG("Required is: ~p~n",[ReqProps]),
            ?LOG("Not Required is: ~p~n",[OptProps]),
            ?LOG("AddProps are: ~p~n", [AddP]),
            ?LOG("PatternProperties are: ~p~n",[PatternProperties]),

            case {PatternProperties,AddP} of
                {undefined,undefined} ->
                    ?LET(N, randIntPositive(MinOpts,length(OptProps)),
                         ?LET(OptPropsGen, choose_n_from_list(OptProps,N),
                              begin
                                  RawProperties = 
                                      [{P,json(S, Options, Depth - 1)} ||
                                          {P,S} <- ReqProps
                                              ++ 
                                              OptPropsGen
                                      ],
                                  case proplists:get_value(randomize_properties,Options,true) of
                                      true ->
                                          ?LET(Props,
                                               randomize_list(RawProperties),
                                               {struct, Props});
                                      false ->
                                          {struct,RawProperties}
                                  end
                              end));

                {PatternProperties, undefined} ->
                    ?LET({N_opt, N_max}, 
                         {randIntPositive(0,length(OptProps)), MaxPropsGen},
                         ?LET(N_prop, randIntPositive(MinOpts - N_opt, (N_max - length(ReqProps)) - N_opt),
                              ?LET({OptPropsGen, PatPropsGen},
                                   {choose_n_from_list(OptProps,N_opt),
                                    create_patterns(PatternProperties,N_prop)},
                                   begin
                                       ?LOG("**FINAL PROPS: ~p~n",
                                            [ReqProps ++ OptPropsGen ++ PatPropsGen]),
                                       RawProperties = 
                                           [{P, json(S, Options, Depth - 1)} ||
                                               {P,S} <- ReqProps
                                                   ++ 
                                                   OptPropsGen
                                                   ++
                                                   lists:concat(PatPropsGen)
                                           ],
                                       case proplists:get_value(randomize_properties,Options,true) of
                                           true ->
                                               ?LET(Props,
                                                    randomize_list(RawProperties),
                                                    {struct, Props});
                                           false ->
                                               {struct,RawProperties}
                                       end
                                   end)));

                {undefined, AddP} ->
                    ?LET({N_opt,N_max},
                         {randIntPositive(0,length(OptProps)), MaxPropsGen},
                         ?LET(N_add, randIntPositive(MinOpts - N_opt, (N_max - length(ReqProps)) - N_opt),
                              ?LET({OptPropsGen, AddPropsGen},
                                   {choose_n_from_list(OptProps, N_opt),
                                    ?LAZY(create_additionals(AddP, N_add))},
                                   begin
                                       ?LOG("**FINAL PROPS: ~p~n",
                                            [ReqProps ++ OptPropsGen ++ AddPropsGen]),
                                       RawProperties = 
                                           [{P, json(S, Options, Depth - 1)} ||
                                               {P,S} <- ReqProps
                                                   ++ 
                                                   OptPropsGen
                                                   ++
                                                   AddPropsGen
                                           ],
                                       ?LOG("AddPropsGen: ~p ~n",[RawProperties]),
                                       case proplists:get_value(randomize_properties,Options,true) of
                                           true ->
                                               ?LET(Props,
                                                    randomize_list(RawProperties),
                                                    {struct, Props});
                                           false ->
                                               {struct,RawProperties}
                                       end
                                   end)));

                {PatternProperties,AddP} ->
                    ?LET({N_opt, N_max},
                         {randIntPositive(0,length(OptProps)), MaxPropsGen},
                         ?LET(N_pat, randIntPositive(0, (N_max - length(ReqProps)) - N_opt),
                              ?LET(N_add,randIntPositive(MinOpts - N_opt - N_pat, 
                                                         (N_max - length(ReqProps)) - N_opt - N_pat),
                                   ?LET({OptPropsGen,PatPropsGen,AddPropsGen},
                                        {choose_n_from_list(OptProps,N_opt),
                                         create_patterns(PatternProperties,N_pat),
                                         ?LAZY(create_additionals(AddP,N_add))},
                                        begin
                                            RawProperties = 
                                                [{P, json(S, Options, Depth - 1)} ||
                                                    {P,S} <- ReqProps
                                                        ++ 
                                                        OptPropsGen
                                                        ++
                                                        lists:concat(PatPropsGen)
                                                        ++
                                                        AddPropsGen
                                                ],
                                            case proplists:get_value(randomize_properties,Options,true) of
                                                true ->
                                                    ?LET(Props,
                                                         randomize_list(RawProperties),
                                                         {struct, Props});
                                                false ->
                                                    {struct,RawProperties}
                                            end
                                        end))))
            end;

        %% string
        %%     A JSON string.
        <<"string">> ->
            MinLength = jsg_jsonschema:keyword(Schema,"minLength"),
            MaxLength = jsg_jsonschema:keyword(Schema,"maxLength"),
            Pattern =  jsg_jsonschema:keyword(Schema,"pattern"), 

            %% Currently we do not like length specifications 
            %% combined with regular expressions. Will this change? 
            %% Maybe, it is not easy to do.

            if
                ((MinLength=/=undefined) orelse (MaxLength=/=undefined)) andalso
                (Pattern=/=undefined) ->

                    throw(regExp_with_length);
                true -> ok
            end,


            if (Pattern == undefined) ->
                    case MinLength of 
                        undefined -> 
                            Min = 0;
                        _ -> 
                            Min = MinLength
                    end,               
                    case MaxLength of
                        undefined ->
                            MaxGen = natural_gte(Min);
                        _ ->
                            MaxGen = MaxLength
                    end,
                    ?LET(Max, MaxGen,
                         ?LET(Rand,randInt(Min,Max), 
                              ?LET(S, stringGen(Rand), list_to_binary(S))));
               true ->
                    ?LOG("Pattern is: ~p~n",[Pattern]),
                    property_name(Pattern)
            end;

        %% any
        %%     Any JSON data, including "null".
        <<"any">> ->
            ?LOG("'any' keyword found",[]),
            ?LET(TypeGen, anyType(),
                 json(TypeGen, Depth - 1));


        %% Union types
        %%     An array of two or more *simple type definitions*.
        Types when is_list(Types) ->
            eqc_gen:oneof
              (lists:map
                 (fun (Type) ->
                          ConcreteSchema = jsg_jsonschema:set_type(Schema,Type),
                          json(ConcreteSchema, Options, Depth -1)
                  end,
                  Types))
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Array gen

-spec array(jsg_json:json_term(),{integer(),integer()},boolean(), integer()) -> eqc_gen:gen(jsg_json:json_term()).
array(Schema,{MinItems,MaxItems}, Unique, Depth) ->
    ?LOG("Array schema is ~p ~n with Unique: ~p~n",[Schema,Unique]),
    case MaxItems of
        undefined ->
            ?LET(Max, natural_gte(MinItems),
                 ?LET(N, eqc_gen:choose(MinItems, Max), 
                      begin
                          case Unique of
                              true ->
                                  arrayGenUnique(Schema, N, Depth);
                              false ->
                                  arrayGen(Schema, N, Depth)
                          end
                      end));
        MaxItems ->
            ?LET(N, eqc_gen:choose(MinItems,MaxItems), 
                 begin
                     case Unique of
                         true ->
                             arrayGenUnique(Schema, N, Depth);
                         false ->
                             arrayGen(Schema, N, Depth)
                     end
                 end)
    end.

                                                % -spec
insertType(Type, {struct,[Types | Rest]}) ->
    ?LOG("insertTypeSchema: ~p",[Types]),
    {<<"type">>, ListOfTypes} = Types,
    ?LOG("Old list of types: ~p ~n",[ListOfTypes]),
    NewListOfTypes = [Type| ListOfTypes],
    ?LOG("New list of types: ~p ~n",[NewListOfTypes]),
    ?LOG("Rest: ~p ~n",[Rest]),
    Res = {struct,[ {<<"type">>, NewListOfTypes} , Rest]},
    ?LOG ("Final res: ~p ~n",[Res]),
    Res.

-spec arrayOfAny(integer(), integer(), boolean(), integer()) -> jsg_json:json_term().
arrayOfAny(MinItems, MaxItems, Unique, Depth) ->
    case MaxItems of
        undefined ->
            ?LET(Max, natural_gte(MinItems),
                 ?LET(N, eqc_gen:choose(MinItems, Max),
                      ?LET(RandType, selectType(),
                           begin
                               NewSchema =  {struct,[{<<"type">>,RandType}]},
                               case Unique of
                                   true ->
                                       arrayGenUnique(NewSchema, N, Depth);
                                   false ->
                                       arrayGen(NewSchema, N, Depth)
                               end
                           end)));

        MaxItems ->
            ?LET(N, eqc_gen:choose(MinItems, MaxItems),
                 ?LET(RandType, selectType(),
                      begin
                          NewSchema =  {struct,[{<<"type">>,RandType}]},
                          case Unique of
                              true ->
                                  arrayGenUnique(NewSchema, N, Depth);
                              false ->
                                  arrayGen(NewSchema, N, Depth)
                          end
                      end))
    end.

-spec arrayGen (jsg_json:json_term(), integer(), integer()) -> eqc_gen:gen(jsg_json:json_term()).
arrayGen(_Schema, _, Depth) when Depth =< 0->
    [];
arrayGen(_Schema, 0, _Depth) ->
    [];

arrayGen(Schema, N, Depth) when N > 0->
    ?LOG("arrayGen: ~p ~n",[Schema]),
    if
        is_list(Schema) ->
            ?LET(Sch, selectSchema(Schema),
                 [json(Sch, [{root, Sch}], Depth - 1) | arrayGen(Schema, N-1, Depth)]);
        true ->
            [json(Schema, [{root, Schema}], Depth - 1) | arrayGen(Schema, N-1, Depth)]
    end.

-spec arrayGenUnique(jsg_json:json_term(), integer(), integer()) -> eqc_gen:gen(jsg_json:json_term()).
arrayGenUnique(_Schema, _, Depth)  when Depth =< 0 ->
    [];

arrayGenUnique(_Schema, 0, _Depth) ->
    [];

arrayGenUnique(Schema,N, Depth) when N > 0->
    ?LOG("arrayGenUnique: ~p ~n",[Schema]),
    if
        is_list(Schema) ->
            ?LET(Sch, selectSchema(Schema),
                 [json(Sch, [{root, Sch}], Depth - 1) | arrayGenUnique(Schema, N-1, Depth) ]);
        true ->
            [ json(Schema, [{root, Schema}], Depth - 1) | arrayGenUnique(Schema, N-1 , Depth)]
    end.

-spec selectSchema([jsg_json:json_term()]) -> jsg_json:json_term().
selectSchema(Schemas) ->
    ?LOG("Array: Selecting type from ~p ~n",[Schemas]),
    eqc_gen:oneof(Schemas).

%% template(_Template) ->
%%     %% TODO: generator for template
%%     null().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Integer generators

-spec integer () -> eqc_gen:gen(integer()).
integer() ->
    eqc_gen:int().

-spec multiple_of (integer()) -> eqc_gen:gen(integer()).
multiple_of(M) ->
    ?LET(N, integer(), M * N).

-spec multiple_of_min(integer(), integer()) -> eqc_gen:gen(integer()).
multiple_of_min(Mul,Min) ->
    MinMul = Mul * (1 + (Min-1) div Mul),
    ?LET(N, nat(), MinMul + Mul * N).

-spec multiple_of_max(integer(), integer()) -> eqc_gen:gen(integer()).
multiple_of_max(Mul,Max) ->
    MaxMul = Mul * (Max div Mul),
    ?LET(N, nat(), MaxMul - Mul * N).

-spec multiple_of_min_max(integer(), integer(), integer()) -> eqc_gen:gen(integer()).
multiple_of_min_max(Mul,Min,Max) ->
    MinMul = (1 + (Min-1) div Mul),
    MaxMul = (Max div Mul),
    ?LET(N, eqc_gen:choose(MinMul,MaxMul), Mul * N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Natural numbers

-spec natural() -> eqc_gen:gen(integer()).
natural() ->
    eqc_gen:nat().

-spec positive() -> eqc_gen:gen(integer()).
positive() ->
    natural_gte(1).

-spec natural_gte(integer()) -> eqc_gen:gen(integer()).
natural_gte(K) ->
    ?LET(N,natural(),N+K).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Number generators

-spec number() -> eqc_gen:gen(integer()) | eqc_gen:gen(float()).
number() ->
    eqc_gen:oneof([eqc_gen:int(),eqc_gen:real()]).

-spec number_positive() -> eqc_gen:gen(integer()) | eqc_gen:gen(float()).
number_positive() ->
    ?SUCHTHAT(N, eqc_gen:oneof([eqc_gen:int(),eqc_gen:real()]), N>=0).

-spec number_mul(integer() | float()) -> eqc:gen_gen(integer()) | eqc:gen_gen(float()).
number_mul(Mul) ->
    ?LET(N, integer(), Mul * N).

-spec number_mul_min(integer() | float(), integer() | float(), boolean()) ->  eqc:gen_gen(integer()) | eqc:gen_gen(float()).
number_mul_min(Mul,Min,MinExc) ->
    MinMul = Mul * (1 + floor( (Min-1) / Mul)),
    case MinExc of
        true ->
            ?SUCHTHAT(N, MinMul + Mul * nat(), N /= Min);

        false ->
            ?LET(N,nat(),MinMul + Mul * N)
    end.

-spec number_mul_max(integer() | float(), integer() | float(), boolean()) ->  eqc:gen_gen(integer()) | eqc:gen_gen(float()).
number_mul_max(Mul,Max,MaxExc) ->
    MaxMul = Mul * (Max div Mul),

    case MaxExc of
        true ->
            ?SUCHTHAT(N, MaxMul - Mul * nat(), N /= Max);
        false ->
            ?LET(N, nat(), MaxMul - Mul * N)
    end.

%% -spec number_mul_min_max(integer() | float(), integer() | float(), integer() | float(), tuple(boolean(), boolean())) ->  eqc:gen_gen(integer()) | eqc:gen_gen(float()).
number_mul_min_max(Mul,Min,Max,{MinExc,MaxExc}) ->
    ?LOG("Min/Mul is ~p~n",[(Min/Mul)]),
    MinMul = case (Min/Mul) == trunc(Min/Mul) of
                                                % if you add 1 unit, Minimum value will never be generated but its inside the valid range
                 true -> 
                     ?LOG("TRUE~n",[]),
                     trunc(Min/Mul); 
                 false -> 
                     ?LOG("FALSE~n",[]),
                     1 + floor(Min/Mul)
             end,
    MaxMul = floor(Max/  Mul),

    ?LOG ("MinMul is ~p~n",[MinMul]),
    case {MinExc,MaxExc} of

        {true,true} ->
            ?SUCHTHAT(N, 
                      ?LET(N, eqc_gen:choose(MinMul,MaxMul), Mul*N),
                      (N =/= Min) and (N =/= Max));

        {true,false} ->
            ?SUCHTHAT(N, 
                      ?LET(N, eqc_gen:choose(MinMul,MaxMul), Mul*N),
                      (N =/= Min));

        {false,true} ->
            ?SUCHTHAT(N, 
                      ?LET(N, eqc_gen:choose(MinMul,MaxMul), Mul*N),
                      (N =/= Max));

        {false,false} ->
            ?LET(N, eqc_gen:choose(MinMul,MaxMul), 
                 Mul * N)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Basic schema generators for additional types

-spec selectType() -> eqc_gen:gen(binary()).
selectType()->
    eqc_gen:oneof([<<"integer">>,<<"string">>,<<"number">>,<<"boolean">>,
                   <<"array">>,<<"object">>,<<"null">>]).

-spec selectSimpleType() -> eqc_gen:gen(binary()).
selectSimpleType() ->
    eqc_gen:oneof([<<"integer">>,<<"string">>,<<"number">>,<<"boolean">>]).

-spec anyType() -> eqc_gen:gen(jsg_json:json_term()).
anyType() ->
    ?LOG("'anyType' -> Choosing random type ('any' keyword)...~n",[]),
    eqc_gen:frequency([{3,stringType()},
                       {3,numberType()},
                       {3,integerType()},
                       {3,booleanType()},
                       {2,nullType()}, 
                       {1,arrayType()},
                       {1,objectType()}]).

-spec stringType() -> jsg_json:json_term().
stringType() ->
    {struct,[{<<"type">>,<<"string">>}]}.

-spec numberType() -> jsg_json:json_term().
numberType() ->
    {struct,[{<<"type">>,<<"number">>}]}.

-spec integerType() -> jsg_json:json_term().
integerType() ->
    {struct,[{<<"type">>,<<"integer">>}]}.

-spec booleanType() -> jsg_json:json_term().
booleanType() ->
    {struct,[{<<"type">>,<<"boolean">>}]}.

-spec objectType() -> jsg_json:json_term().
objectType() ->
    ?LAZY(?LET(RandType, selectType(),
               {struct,[{<<"type">>,<<"object">>},{<<"additionalProperties">>,
                                                   {struct,[{<<"type">>,RandType}]}}]})).

-spec arrayType() -> jsg_json:json_term().
arrayType() ->
    ?LAZY(?LET(RandType, selectType(),
               {struct,[{<<"type">>,<<"array">>},
                        {<<"additionalItems">>,false},
                        {<<"items">>,{struct,[{<<"type">>,RandType}]}}]})).

-spec nullType() -> jsg_json:json_term().
nullType() ->
    {struct,[{<<"type">>,<<"null">>}]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Random functions

randString() ->
    ?LET(N, positive(),
         ?LET(S,stringGen(N), list_to_binary(S))).

-spec stringGen(integer()) -> eqc_gen:gen(string()).
stringGen(0) ->
    [];

stringGen(N) ->
    ?LET({S,G},{eqc_gen:choose($a,$z), stringGen(N-1)}, [S|G]).


-spec randInt(integer(),integer()) -> eqc_gen:gen(integer()).
randInt (Min,Max) ->

    case {Min,Max} of 

        {undefined,undefined} -> 
            positive();
        {undefined,Max} ->
            eqc_gen:choose(1,Max);
        {Min,undefined} ->
            natural_gte(Min);
        {Min,Max} -> 
            eqc_gen:choose(Min,Max)
    end.

-spec randIntPositive(integer(), integer()) -> eqc_gen:gen(integer()).
randIntPositive(Min,Max) when (Min > 0) and (Max > 0) ->
    case {Min,Max} of 

        {undefined,undefined} -> 
            positive();
        {undefined,Max} ->
            eqc_gen:choose(1,Max);
        {Min,undefined} ->
            natural_gte(Min);
        {Min,Max} -> 
            eqc_gen:choose(Min,Max)
    end;

randIntPositive(_Min,Max) when Max < 0 ->
    0;

randIntPositive(_Min,Max) ->
    eqc_gen:choose(0,Max).


-spec randFlt(float(), float()) -> eqc_gen:gen(float()).
randFlt (Min, _) ->
    ?LET(Flt, eqc_gen:real(), Min + Flt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generators for boolean and null

-spec boolean() -> eqc_gen:gen(boolean()).
boolean() ->
    eqc_gen:bool().

-spec null() -> atom().
null() ->
    null.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generators for choosing random properties from a list

-spec choose_from_list([string()],integer(),integer()) -> eqc_gen:gen([string()]).
choose_from_list(P, Min, Max) when Max >= Min -> 
    ?LOG("Choosing properties: ~p~n",[P]),
    ?LOG("Min, Max: ~p,~p~n",[Min,Max]),
    ?LET(N, eqc_gen:choose (max(0,Min),Max), choose_n_from_list(P,N)).

-spec choose_n_from_list([jsg_json:json_term()], integer()) -> [jsg_json:json_term()].
choose_n_from_list(L,N) ->
    randomize_list(L,N,length(L)).

-spec randomize_list([jsg_json:json_term()]) -> [jsg_json:json_term()].
randomize_list(L) ->
    Length = length(L),
    randomize_list(L,Length,Length).
randomize_list([],_,_Length) -> [];
randomize_list(_List,0,_Length) -> [];
randomize_list(List, N, Length) ->
    ?LET(I, eqc_gen:choose(1, Length), 
         [lists:nth(I,List) |
          randomize_list(delete_nth_element(I,List), N-1, Length-1)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generators for patternProperties keyword

-spec property_name(string()) -> eqc_gen:gen(string()).
property_name(Pattern) ->
    ?LOG("Pattern name: ~p~n",[Pattern]),
    RegularExpression = binary_to_list(Pattern),
    InternalRegularExpression = jsg_regexp_parse:string(RegularExpression),
    ?LET
       (String,
        jsg_gen_string_from_regexp:gen(RegularExpression, InternalRegularExpression),
        list_to_binary(String)).

-spec pattern_gen({string(), jsg_json:json_term()}, integer()) -> [{string(), jsg_json:json_term()}].
pattern_gen(_,0) ->
    [];
pattern_gen({Pattern, Schema},N) when N > 0 ->
    ?LOG("{Pattern, schema} = ~p~n", [{Pattern,Schema}]),
    [{property_name(Pattern), Schema} | pattern_gen({Pattern,Schema}, N-1)].

pattern_gen(Pattern_Schema) ->
    ?LOG("{Pattern_schema} = ~p~n", [Pattern_Schema]),
    ?LET(N,natural(), pattern_gen(Pattern_Schema,N)).

-spec pattern_gen_range([string()], integer()) -> eqc_gen:gen([string()]). 
pattern_gen_range(Pattern_Schema, Min) ->
    ?LOG("{Pattern_schema} = ~p~n", [Pattern_Schema]),
    ?LET(N,natural_gte(Min), pattern_gen(Pattern_Schema,N)).

-spec create_patterns(undefined | [string()])-> ([string] | []).
create_patterns(undefined) ->
    [];

create_patterns(PatternPropList) ->
    ?LOG("Inside create_patterns, PatternPropList is ~p~n",[PatternPropList]),
    L = lists:map (fun(X) -> pattern_gen(X) end, PatternPropList),
    ?LOG("Final patterns created: ~p~n",[L]),
    L.

-spec create_patterns((undefined | [string()]), integer()) -> [string()].
create_patterns(undefined,_) ->
    [];

create_patterns(PatternPropList, MinimumProps) ->
    ?LOG("create_patterns with minmum, PatternPropList is ~p, and Min is ~p~n",
         [PatternPropList, MinimumProps]),
    Min = ceiling(MinimumProps / length(PatternPropList)),
    L = lists:map (fun(X) -> pattern_gen_range(X,Min) end, PatternPropList),
    ?LOG("Final patterns created: ~p~n",[L]),
    L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generators for additionalProperties keyword

-spec additional_gen (jsg_json:json_term() , integer()) -> string().
additional_gen(_, N) when N =< 0 ->
    [];

additional_gen(AdditionalSchema,N) when N > 0 ->
    case AdditionalSchema of
        {} ->
            ?LOG("Empty schema~n",[]),
            [{randString(), anyType()} | additional_gen(AdditionalSchema, N - 1)];
        Schema ->
            ?LOG("Not empty schema, type is {randString(),~p}~n",[Schema]),
            [{randString(), {struct,[Schema]}} | additional_gen(AdditionalSchema, N - 1)]
    end.

-spec create_additionals(jsg_json:json_term(), integer()) -> [string()].
create_additionals(O, N) ->
    create_additionals_(O, N).

-spec create_additionals_(jsg_json:json_term(), integer()) -> [string()].
create_additionals_({}, N) ->
    additional_gen({}, N);

create_additionals_({struct, AddPropList}, N) ->
    FinalProps = ceiling(N / length(AddPropList)),
    ?LOG ("Final Props: ~p~n",[FinalProps]),
    L = lists:map (fun(X) -> additional_gen(X,FinalProps) end, AddPropList),
    ?LOG("Final additionals created: ~p~n",[L]),
    lists:concat(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Misc functions

-spec floor((integer() | float())) -> integer().
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;

floor(X) ->
    trunc(X).

-spec delete_nth_element(integer(), [string()]) -> [string()].
delete_nth_element(N, List) ->
    ?LOG("Removing ~p element from list -> ~p~n",[N,List]),
    delete_nth_element(N-1,List, []).

delete_nth_element(0, [_nthEl|T], Res) ->
    concat_and_reverse(T, Res);

delete_nth_element(N, [H|T], Res) ->
    delete_nth_element(N-1, T, [H|Res]).

-spec concat_and_reverse(List, List) -> List.  %% mejor dejar lista a secas sin especificar??
concat_and_reverse([],Res) ->
    lists:reverse(Res);

concat_and_reverse([H|T], Res) ->
    concat_and_reverse(T, [H|Res]).

-spec ceiling(integer() | float()) -> integer().
ceiling(X) ->
    T = trunc(X),
    case (X - T) of
        Negative when Negative < 0 -> T;
        Positive when Positive > 0 -> T + 1;
        _ -> T
    end.

version() ->
    case [ Vsn || {jsongen, _, Vsn} <- application:loaded_applications() ] of
        [] ->
            "unknown";
        [Vsn] ->
            Vsn
    end.


