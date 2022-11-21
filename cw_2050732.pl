% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), 
    get_agent_position(A,P),
    (achieved(Task,P) -> true
    ;otherwise   -> search_a_star(Task, [[arc(0,P)]], [], Path),
                    agent_do_moves(A,Path),
                    length(Path,Cost)).

search_a_star(Task,[Next|Rest],Visited,Path) :-
    Next = [Pos|RPath],
    Pos = arc(C,P),
    (achieved(Task,P) -> reverse([Pos|RPath],[_|WPath]), formatPath(WPath, Path)
    ;otherwise     -> (findall([arc(NC,NP),Pos|RPath],
                               (map_adjacent(P,NP,empty), 
                               length([Pos|RPath], G),
                               manhattan_distance(NP, Task, H), NC is G + H,
                               \+ member_of_list(arc(NC,NP),Rest),
                               \+ member_of_list(arc(NC,NP),Visited)),
                               Children),
                      append(Rest,Children,NewQueue),
                      sort(NewQueue, SortedQueue),
                      search_a_star(Task,SortedQueue,[Pos|Visited],Path))).

manhattan_distance(p(X,Y), go(p(X1,Y1)), H) :-
    H is abs(X - X1) + abs(Y - Y1),!.
manhattan_distance(_, _, H) :- H is 0.

formatPath([], []).
formatPath([arc(C,P)|T], [P|T1]) :- formatPath(T, T1).

member_of_list(p(X,Y)), []) :- false.
member_of_list(arc(C,p(X,Y)), [arc(C1,p(X1,Y1))|T]) :- 
    X == X1, Y == Y1, C1 =< C
    ; 
    member_of_list(arc(C,p(X,Y)), T).

print_list([]) :- nl.
print_list([H|T]) :- write(H), write(", "), print_list(T).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task=go(Pos).