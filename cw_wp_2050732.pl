actor_has_link(L,A) :-
actor(A), wp(A,WT), wt_link(WT,L).

% Returns Actor if it has narrowed it down to one 
% Else, find shortest paths to each oracle and charging station from current agent position
eliminate(As,A,Oracles, Stations) :-
    As=[A], !
    ;
    my_agent(N),
    get_agent_position(N, Pos),
    get_paths_to_oracles(Pos, Oracles, OPaths),
    get_paths_to_stations(Pos, Stations, SPaths),
    eliminate(As,A,Oracles, Stations, OPaths, SPaths).

% Based on the closest oracle, see if you need to top up before visiting it. If so, top up
% Else, go to the cloest oracle, query it and run eliminate again
eliminate(As,A,Oracles,Stations,OPaths,SPaths) :-
    my_agent(N),
    get_agent_energy(N, E),
    nth1(1, OPaths, (OLen, OraclePosition, OPath)),
    nth1(1, SPaths, (SLen, StationPosition, SPath)),
    (OLen == 0 -> get_agent_position(N, OP) 
    ; otherwise -> last(OPath, OP)),
    (should_topup(SLen, OLen, OP, Stations)-> 
        agent_do_moves(N, SPath),
        lookup_pos(StationPosition, c(K)),
        agent_topup_energy(N, c(K)),
        eliminate(As,A,Oracles,Stations)
    ; otherwise ->
        agent_do_moves(N, OPath),
        lookup_pos(OraclePosition, o(K)),
        agent_ask_oracle(N,o(K),link,L),
        delete(Oracles, OraclePosition, NOracles),
        include(actor_has_link(L),As,ViableAs),
        eliminate(ViableAs,A,NOracles,Stations)
    ).

% Identify positions of all oracles and charging stations on the grid 
% Call eliminate until it returns an Actor 
% Or fails after exhausting all oracles or running out of energy, and then return unknown
find_identity(A) :-
    findall(A,actor(A),As), 
    find_all_oracles((Oracles, OPaths)),
    find_all_stations((Stations, SPaths)),
    eliminate(As,A,Oracles,Stations, OPaths, SPaths)
    ; otherwise -> A = unknown.

% Every move you make, you want to make sure you will have enough energy to visit a charging station if needed
% SLen \= 0 checks if it is already at a charging station, stops infinite loop of topping up energy
% SLen =< E if agent doesnt have enough energy to go to charging station, dont bother going to it
% See how far a charging station will be away from you if you make this move
% Check if you have enough energy to visit the oracle, query it and go to the nearest charging station from it
% If yes, dont topup, Else topup and rethink the next move
should_topup(SLen, OLen, OraclePosition, Stations) :-
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

% Using A* search to find locations to each charging station and the paths to them from the agents current position
find_all_stations((Stations, SPaths)) :-
    my_agent(A),
    get_agent_position(A,Pos),
    convlist(search_for_node([[arc(0,Pos)]], []), [c(1),c(2),c(3),c(4)], List),
    sort(List, SPaths),
    get_node_list(SPaths, Stations).

get_paths_to_stations(Pos, Stations, Paths) :-
    convlist(path_to_station(Pos), Stations, Ps),
    sort(Ps, Paths).

% Using A* to find the fastest path from Pos to the StationPosition
path_to_station(Pos, StationPosition, (L, StationPosition, Path)) :-
    search_a_star_node([[arc(0,Pos)]], [], StationPosition,Path),
    length(Path, L).

% Using A* search to find locations to each oracle and the paths to them from the agents current position
find_all_oracles((Oracles, SPaths)) :-
    my_agent(A),
    get_agent_position(A,Pos),
    convlist(search_for_node([[arc(0,Pos)]], []), [o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10)], List),
    sort(List, SPaths),
    get_node_list(SPaths, Oracles).

get_paths_to_oracles(Pos, Oracles, Paths) :-
    convlist(path_to_oracle(Pos), Oracles, Ps),
    sort(Ps, Paths).

% Using A* to find the fastest path from Pos to the OraclePosition
path_to_oracle(Pos, OraclePosition, (L, OraclePosition, Path)) :-
    search_a_star_node([[arc(0,Pos)]], [], OraclePosition, Path),
    length(Path, L).

% A* search which executes find(c(K)) or find(o(K)) to find the position and path to a particular oracle or charging station
search_for_node([Next|Rest],Visited, Obj, (L, Position, Path)) :-
    Next = [Pos|RPath],
    Pos = arc(C,P),
    (map_adjacent(P, Position, Obj) -> reverse([Pos|RPath],[_|WPath]), formatPath(WPath, Path), length(Path, L)
    ;otherwise     -> (findall([arc(NC,NP),Pos|RPath],
                               (map_adjacent(P,NP,empty), 
                               length([Pos|RPath], NC),
                               \+ member_of_rest(arc(NC,NP),Rest),
                               \+ member_of_list(arc(NC,NP),Visited)),
                               Children),
                      append(Rest,Children,NewQueue),
                      sort(NewQueue, SortedQueue),
                      search_for_node(SortedQueue,[Pos|Visited], Obj, (L, Position, Path)))).

% A* search search which finds the fastest path to Position
% Uses map_adjacent instead of achieved to determine if it has found the path
% Because you cannot navigate to the oracle position, only be adjacent to it
search_a_star_node([Next|Rest],Visited, Position, Path) :-
    Next = [Pos|RPath],
    Pos = arc(C,P),
    (map_adjacent(P, Position, _) -> reverse([Pos|RPath],[_|WPath]), formatPath(WPath, Path)
    ;otherwise     -> (findall([arc(NC,NP),Pos|RPath],
                               (map_adjacent(P,NP,empty), 
                               length([Pos|RPath], G),
                               manhattan_distance(NP, go(Position), H), NC is G + H,
                               \+ member_of_rest(arc(NC,NP),Rest),
                               \+ member_of_list(arc(NC,NP),Visited)),
                               Children),
                      append(Rest,Children,NewQueue),
                      sort(NewQueue, SortedQueue),
                      search_a_star_node(SortedQueue,[Pos|Visited],Position,Path))).

% Extract list of Positions from list of multiple elements
get_node_list([], []).
get_node_list([(_, Position, _)|T], [Position|T1]) :- get_node_list(T, T1).
