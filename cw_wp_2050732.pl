% True if link L appears on A's wikipedia page
actor_has_link(L,A) :- 
    actor(A), wp(A,WT), wt_link(WT,L).

eliminate(As, A, Oracles, Paths) :-
    As=[A], !
    ;
    ailp_grid_size(S),
    EMAX is S * S / 4,
    EASK is EMAX / 10,
    my_agent(N),
    get_agent_position(N,P),
    get_agent_energy(1,E),
    (E =< 20 ->
    convlist(find_charging_station(P), [1,2,3,4], ChargingStations),
    sort(ChargingStations, SChargingStations),
    nth0(0, SChargingStations, charging(CLen, CK, CPath)),
    agent_do_moves(N, CPath),
    agent_topup_energy(N,c(CK)),
    eliminate(As, A, UnChecked)
    ; otherwise ->
    sort(Paths, SPaths),
    nth0(0, SPaths, oracle(Len, K, Pos, Path)),
    agent_do_moves(N,Path),
    agent_ask_oracle(N, o(K), link, L),
    delete(Oracles, Pos, NOracles),
    include(actor_has_link(L),As,NewAs), 

    eliminate(NewAs, A, NOracles)).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    findall(A,actor(A),As), 
    find_all_oracles(Oracles, Paths),
    eliminate(As,A, Oracles, Paths).

find_all_oracles(Oracles, Paths) :-
    my_agent(N),
    get_agent_position(N,P),
    convlist(find_oracle(P), [1,2,3,4,5,6,7,8,9,10], Paths),
    get_oracle_locations(Paths, Oracles).

find_charging_station(P, K, charging(L, K, Path)) :- 
    search(P, find(c(K)), Path),
    length(Path, L).

find_oracle(P, K, oracle(L, K, Pos, Path)) :- 
    search(P, find(o(K)), Path),
    last(Path, Pos),
    length(Path, L).

get_oracle_locations([], []).
get_oracle_locations([oracle(_,_,Pos,_)|T], [Pos|T1]) :- get_oracle_locations(T, T1).