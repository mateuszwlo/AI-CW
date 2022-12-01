% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze() :-
    my_agents(As),
    last(As, A),
    solve(A, [p(1,1)], []).

solve(A, Visited, Pivots) :-
    leave_maze(A) -> true
    ;otherwise ->
    findall((W,p(X,Y)), (agent_adjacent(A,p(X,Y),empty), \+ member(p(X,Y),Visited), W is X + Y), AvailableMoves),
    length(AvailableMoves, L),
    (L == 0 -> (head(Pivots, Pivot),
    get_agent_position(A, Pos),
    search(Pos, go(Pivot), Path),
    agent_do_moves(A, Path),
    tail(Pivots, NPivots),
    solve(A, Visited, NPivots))
    ;otherwise -> (L > 0 -> sort(AvailableMoves, SMoves),reverse(SMoves,Moves),nth1(1,Moves,(W,Pos)),    
    get_agent_position(A, P),
    agent_do_moves(A, [Pos]),
    append([Pos], Visited, NVisisted),
    (L > 1 -> append([P], Pivots, NPivots), solve(A, NVisisted, NPivots) 
    ;otherwise -> solve(A, NVisisted, Pivots)
    )
    )).

tail([H|T], T).
head([H|T], H).