% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze() :-
    my_agents(As),
    get_agent_positions(As, Visited),
    solve(As, Visited, []).

solve([A|As], Visited, Pivots) :-
    leave_maze(A) -> true
    ;otherwise ->
    head(Visisted, HVisited),
    tail(Visisted, TVisited),
    findall(P, (agent_adjacent(A,P,empty), \+ member(P,HVisited)), AvailableMoves),
    length(AvailableMoves, L),
    (L == 0 -> (head(Pivots, Pivot),
    get_agent_position(A, Pos),
    search(Pos, go(Pivot), Path),
    agent_do_moves(A, Path),
    tail(Pivots, NPivots),
    other_agents(As, TVisited, NVisisted),
    append(HVisited, NVisisted, Visisted1),
    solve([A|As], Visited1, NPivots))
    ;otherwise -> 
    (L > 0 -> random_member(Pos, AvailableMoves),    
    get_agent_position(A, P),
    agent_do_moves(A, [Pos]),
    append([Pos], Visited, NVisisted),
    (L > 1 -> append([P], Pivots, NPivots),     
    other_agents(As, TVisited, NVisisted),
    append(HVisited, NVisisted, Visisted1), 
    solve([A|As], Visisted1, NPivots) 
    ;otherwise -> other_agents(As, TVisited, NVisisted),
    append(HVisited, NVisisted, Visisted1),
    solve([A|As], Visisted1, Pivots)
    )
    )).

other_agents([], [], []).
other_agents([A|As], [V|T], [V1|Visited]) :-
    findall(P, (agent_adjacent(A,P,empty), \+ member(P, V)), AvailableMoves),
    random_member(M,AvailableMoves),
    agent_do_moves(A, [M]),
    append(V, [M], V1),
    other_agents(As, T, Visisted).

get_agent_positions([], []).
get_agent_positions([A|As], [[Pos]|Positions]) :-
    get_agent_position(A, Pos),
    get_agent_positions(As, Positions).

find_moves([], Visited, []).
find_moves([A|As], Visited, [AvailableMoves|Moves]) :-
    findall(P, (agent_adjacent(A,P,empty), \+ member(P,Visited)), AvailableMoves),
    find_moves(As, Visited, Moves).

tail([H|T], T).
head([H|T], H).