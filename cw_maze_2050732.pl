% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze() :-
    my_agents(As),
    reverse(As, Agents),
    convlist(get_agent_position, Agents, Visited),
    find_moves(Agents, Visited, []).

% Find the best move for Agent A to make
solve(A, Visited, Pivots, NVisisted, NPivots) :-
% Find adjacent squares which no other agents have visited
    findall((W,p(X,Y)),(agent_adjacent(A,p(X,Y),empty),\+ member(p(X,Y),Visited),W is X + Y),AvailableMoves),
    length(AvailableMoves, L),
    length(Pivots, PLen),

% As the exit is at p(N,N), pick the square closest to the exit to explore first
% Navigate to this square and add it to the visited list

    (L > 0 -> 
        sort(AvailableMoves, SMoves),
        reverse(SMoves,Moves),
        nth1(1,Moves,(W,Pos)),    
        get_agent_position(A, P),
        agent_do_moves(A, [Pos]),
        append([Pos], Visited, NVisisted),

% If there is more than 1 direction you can go from this current position, it is a pivot point 
% Add this position to the pivot list, either this agent or another one will explore in the other direction 
    
    (L > 1 -> 
        H is L - 1, append([pivot(H, P)], Pivots, NPivots)
    ; otherwise -> NPivots = Pivots
    )

% If Agent A has no available moves, visit the nearest pivot point to it
% Some pivot points can have multiple directions to explore
% Subtract 1 from the directions to explore counter and remove from the list if all directions have been explored
    
    ; otherwise -> 
        (PLen == 0 -> NVisisted = Visited, NPivots = Pivots
        ; otherwise -> 
            get_agent_position(A, Pos),
            (closest_pivot(Pos, Pivots, (K, Pivot, Path)) ->
                agent_do_moves(A, Path),
                delete(Pivots, pivot(K, Pivot), DPivots),
                H is K - 1,
                NVisisted = Visited,
                (H > 0 -> append([pivot(H, Pivot)], DPivots, NPivots)
                ; otherwise -> NPivots = DPivots)
            ; otherwise -> NVisisted = Visited, NPivots = Pivots)
        )
    ).

% Find paths to each pivot point from Pos using search algorithm from part 1
% Return information from the closest one
closest_pivot(Pos, Pivots, (K, Pivot, Path)) :-
    convlist(path_to_pivot(Pos), Pivots, Paths),
    sort(Paths, SPaths),
    nth1(1, SPaths, (L, K, Path)),
    last(Path, Pivot).

path_to_pivot(Pos, pivot(K, PivotPosition), (L, K, Path)) :-
    search(Pos, go(PivotPosition), Path),
    length(Path, L).

% Looped over all agents so continue looping over them
find_moves([], Visited, Pivots) :- 
    my_agents(As), 
    reverse(As, Agents),
    find_moves(Agents, Visited, Pivots).

% Iterates over all agents, find their best move and executes it
% If exit is found by any agent, navigates the remaining ones to it
find_moves([A|As], Visited, Pivots) :-
    leave_maze(A) -> my_agents(Agents), go_to_exit(Agents), !
    ; otherwise -> 
    solve(A, Visited, Pivots, NVisisted, NPivots),
    find_moves(As, NVisisted, NPivots).

% Guide all agents to the exit when found
% Find the path using search algorithm from part 1
% If cant find path due to another agent blocking it, move onto the next agent and try again after it has been unblocked
go_to_exit([]).
go_to_exit([A|T]) :-
    get_agent_position(A, P),
    ailp_grid_size(S),
    (search(P, go(p(S,S)), Path) -> 
        agent_do_moves(A, Path),
        leave_maze(A),
        go_to_exit(T)
    ; otherwise -> append(T, [A], T1), go_to_exit(T1)
    ).