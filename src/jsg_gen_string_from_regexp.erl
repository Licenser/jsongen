-module(jsg_gen_string_from_regexp).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

gen(Regexp, R) ->
    gen_(R).
    %% ?SUCHTHAT(
    %%    S,
    %%    gen_(R),
    %%    re:run(S, Regexp) =/= nomatch
    %%   ).

gen_({'or',L}) ->
  eqc_gen:oneof(lists:map(fun gen_/1, L));
gen_({'concat',R1,R2}) ->
  ?LET
     ({X,Y},
      {gen_(R1),gen_(R2)},
      X++Y);

gen_({'symbol',S}) ->
  [S];
gen_({'characterClass',CSpec}) ->
  gen_characterClass_element(CSpec);
gen_({quantify,R,N}) ->
  ?LET(FA,calculate_arity(N),
       gen_({fixed,FA,R}));
gen_({fixed,0,_R}) ->
  "";
gen_({fixed,N,R}) ->
  gen_({concat,R,{fixed,N-1,R}}).

gen_characterClass_element(CSpec) ->
  eqc_gen:oneof(lists:map(fun gen_spec_element/1, CSpec)).

gen_spec_element({'range',pos,{'symbol',S1},{'symbol',S2}}) ->
  [eqc_gen:choose(S1,S2)];
gen_spec_element({'range',neg,{'symbol',S1},{'symbol',S2}}) ->
  [eqc_gen:oneof
     ([eqc_gen:choose(0,S1-1),
       eqc_gen:choose(S2+1,127)])];
gen_spec_element({'range',pos,L}) ->
  [eqc_gen:oneof(lists:map(fun ({symbol,X}) -> X end,L))];
gen_spec_element({'range',neg,L}) ->
  NegList =
    sets:to_list
      (sets:subtract(sets:from_list(lists:seq(0,127)),
		     sets:from_list(lists:map(fun ({symbol,X}) -> X end, L)))),
  [eqc_gen:oneof(NegList)].

calculate_arity(star) ->
  nat();
calculate_arity(plus) ->
  ?LET(N,nat(),N+1);
calculate_arity('query') ->
  eqc_gen:choose([0,1]);
calculate_arity({number,N}) ->
  N;
calculate_arity({number_comma,N}) ->
  ?LET(M,nat(),N+M);
calculate_arity({number_number,N,M}) ->
  eqc_gen:choose(N,M).
