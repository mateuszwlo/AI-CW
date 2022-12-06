% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze() :-
    my_agents(As),
    reverse(As, Agents),
    convlist(get_agent_position, Agents, Visited),
    find_moves(Agents, Visited, []).

solve(A, Visited, Pivots, NVisisted, NPivots) :-
    findall((W,p(X,Y)), (agent_adjacent(A,p(X,Y),empty), \+ member(p(X,Y),Visited), W is X + Y), AvailableMoves),
    length(AvailableMoves, L),
    length(Pivots, PLen),
    (L > 0 -> 
        write("L > 0"),
        nl,
        sort(AvailableMoves, SMoves),
        reverse(SMoves,Moves),
        nth1(1,Moves,(W,Pos)),    
        get_agent_position(A, P),
        agent_do_moves(A, [Pos]),
        append([Pos], Visited, NVisisted),
    (L > 1 -> 
        H is L - 1, append([pivot(H, P)], Pivots, NPivots), 
        write("L > 1"), 
        nl
    ; otherwise -> NPivots = Pivots
    )
    ; otherwise -> 
        write("L == 0 "),
        nl,
        (PLen == 0 -> NVisisted = Visited, NPivots = Pivots, write("PLen == 0"), nl
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

closest_pivot(Pos, Pivots, (K, Pivot, Path)) :-
    convlist(path_to_pivot(Pos), Pivots, Paths),
    sort(Paths, SPaths),
    nth1(1, SPaths, (L, K, Path)),
    last(Path, Pivot).

path_to_pivot(Pos, pivot(K, PivotPosition), (L, K, Path)) :-
    search(Pos, go(PivotPosition), Path),
    length(Path, L).

find_moves([], Visited, Pivots) :- 
    my_agents(As), 
    reverse(As, Agents),
    find_moves(Agents, Visited, Pivots).

find_moves([A|As], Visited, Pivots) :-
    write("Agent "),
    write(A),
    nl,
    write("Visited: "), print_list(Visited),
    write("Pivots: "), print_list(Pivots),
    (leave_maze(A) -> my_agents(Agents), go_to_exit(Agents), !
    ; otherwise -> 
    solve(A, Visited, Pivots, NVisisted, NPivots),
    find_moves(As, NVisisted, NPivots)).

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

tail([H|T], T).
head([H|T], H).