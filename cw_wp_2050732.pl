actor_has_link(L,A) :-
actor(A), wp(A,WT), wt_link(WT,L).

eliminate(As,A,Oracles, Stations) :-
    write("eliminate(As,A,Oracles, Stations)"),
    nl,
    As=[A], !
    ;
    my_agent(N),
    get_agent_position(N, Pos),
    get_paths_to_oracles(Pos, Oracles, OPaths),
    get_paths_to_stations(Pos, Stations, SPaths),
    eliminate(As,A,Oracles, Stations, OPaths, SPaths).

eliminate(As,A,Oracles,Stations,OPaths,SPaths) :-
    write("eliminate(As,A,Oracles,Stations,OPaths,SPaths)"),
    nl,
    my_agent(N),
    get_agent_energy(N, E),
    get_agent_position(N, Pos),
    write(Pos),
    write(" "),
    write(E),
    nl,
    nth1(1, OPaths, (OLen, OraclePosition, OPath)),
    nth1(1, SPaths, (SLen, StationPosition, SPath)),
    (OLen == 0 -> get_agent_position(N, OP) 
    ; otherwise -> last(OPath, OP)),
    (should_topup(SLen, OLen, OP, Stations)-> 
        write("topping up"),
        nl,
        agent_do_moves(N, SPath),
        lookup_pos(StationPosition, c(K)),
        agent_topup_energy(N, c(K)),
        eliminate(As,A,Oracles,Stations)
    ; otherwise ->
        write("going to oracle"),
        nl,
        agent_do_moves(N, OPath),
        lookup_pos(OraclePosition, o(K)),
        agent_ask_oracle(N,o(K),link,L),
        delete(Oracles, OraclePosition, NOracles),
        include(actor_has_link(L),As,ViableAs),
        eliminate(ViableAs,A,NOracles,Stations)
    ).

find_identity(A) :-
    write("find_identity"),
    nl,
    findall(A,actor(A),As), 
    find_all_oracles((Oracles, OPaths)),
    find_all_stations((Stations, SPaths)),
    eliminate(As,A,Oracles,Stations, OPaths, SPaths).

should_topup(SLen, OLen, OraclePosition, Stations) :-
    write("should_topup"),
    nl,
    SLen \= 0,
    my_agent(N),
    get_agent_energy(N,E),
    SLen =< E,
    ailp_grid_size(S),
    MAX is S * S / 4,
    round(MAX, EMAX),
    ASK is EMAX / 10,
    round(ASK, EASK),
    get_paths_to_stations(OraclePosition, Stations, SPaths),
    nth1(1, SPaths, (Len, _, _)),
    H is EASK + OLen + Len,
    H > E.

find_all_stations((Stations, SPaths)) :-
    write("find_all_stations((Stations, SPaths))"),
    nl,
    my_agent(A),
    get_agent_position(A,Pos),
    convlist(search_for_node([[Pos]], []), [c(1),c(2),c(3),c(4)], List),
    sort(List, SPaths),
    get_node_list(SPaths, Stations).

get_paths_to_stations(Pos, Stations, Paths) :-
    write("get_paths_to_stations(Pos, Stations, Paths)"),
    nl,
    convlist(path_to_station(Pos), Stations, Ps),
    sort(Ps, Paths).

path_to_station(Pos, StationPosition, (L, StationPosition, Path)) :-
    write("path_to_station(Pos, StationPosition, (L, StationPosition, Path))"),
    nl,
    search_bf_node([[Pos]], [], StationPosition,Path),
    length(Path, L).

find_all_oracles((Oracles, SPaths)) :-
    write("find_all_oracles((Oracles, SPaths))"),
    nl,
    my_agent(A),
    get_agent_position(A,Pos),
    convlist(search_for_node([[Pos]], []), [o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10)], List),
    sort(List, SPaths),
    get_node_list(SPaths, Oracles).

get_paths_to_oracles(Pos, Oracles, Paths) :-
    write("get_paths_to_oracles(Pos, Oracles, Paths)"),
    nl,
    convlist(path_to_oracle(Pos), Oracles, Ps),
    sort(Ps, Paths).

path_to_oracle(Pos, OraclePosition, (L, OraclePosition, Path)) :-
    write("path_to_oracle("),
    write(Pos),
    write(","),
    write(OraclePosition),
    write(", (L, OraclePosition, Path))"),
    nl,
    search_bf_node([[Pos]], [], OraclePosition, Path),
    length(Path, L).

search_for_node([Next|Rest], Visited, Obj, (L, Position, Path)) :-
    Next = [Pos|RPath],
    (map_adjacent(Pos, Position, Obj) -> reverse([Pos|RPath],[_|Path]), length(Path, L)
    ;otherwise     -> (findall([NP,Pos|RPath],
                               (map_adjacent(Pos,NP,empty),
                               \+ member(NP,Visited), 
                               \+ member([NP|_],Rest)),
                               Newfound),
                      append(Rest,Newfound,NewQueue),
                      search_for_node(NewQueue, [Pos|Visited], Obj, (L, Position, Path)))).

search_bf_node([Next|Rest], Visited, Position, Path) :-
    Next = [Pos|RPath],
    (map_adjacent(Pos, Position, _) -> reverse([Pos|RPath],[_|Path])
    ;otherwise     -> (findall([NP,Pos|RPath],
                               (map_adjacent(Pos,NP,empty),
                               \+ member(NP,Visited), 
                               \+ member([NP|_],Rest)),
                               Newfound),
                      append(Rest,Newfound,NewQueue),
                      search_bf_node(NewQueue, [Pos|Visited], Position, Path))).

get_node_list([], []).
get_node_list([(_, Position, _)|T], [Position|T1]) :- get_node_list(T, T1).
