% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), 
    get_agent_position(A,P),
    (achieved(Task,P) -> true
    ;otherwise   -> search(P, Task, Path),
                    agent_do_moves(A,Path),
                    length(Path,Cost)).

search(P, Task, Path) :- search_a_star(Task, [[arc(0,P)]], [], Path).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task=go(Pos).

% A* search algorithm
% Pick the node with the least cost, find all of its neighbours which havent been explored
% Calculate a heuristic for them and add them to the queue and sort the queue
% Rerun this until it achieves the Task and then return the Path
search_a_star(Task,[Next|Rest],Visited,Path) :-
    Next = [Pos|RPath],
    Pos = arc(C,P),
    (achieved(Task,P) -> reverse([Pos|RPath],[_|WPath]), formatPath(WPath, Path)
    ;otherwise     -> (findall([arc(NC,NP),Pos|RPath],
                               (map_adjacent(P,NP,empty), 
                               length([Pos|RPath], G),
                               manhattan_distance(NP, Task, H), NC is G + H,
                               \+ member_of_rest(arc(NC,NP),Rest),
                               \+ member_of_list(arc(NC,NP),Visited)),
                               Children),
                      sort(Children, SChildren),
                      ord_union(Rest, SChildren, SortedQueue),
                      search_a_star(Task,SortedQueue,[Pos|Visited],Path))).

% If Task is not in the form go(P), then cannot calculate a heuristic
manhattan_distance(p(X,Y), go(p(X1,Y1)), H) :-
    H is abs(X - X1) + abs(Y - Y1),!.
manhattan_distance(_, _, H) :- H is 0.

% Format list of nodes so it returns a list of positions only and can be used as a Path
formatPath([], []).
formatPath([arc(_,P)|T], [P|T1]) :- formatPath(T, T1).

% Check if node already in queue, and if it is check if it has less cost than the new one 
% If it does, return true, hence stopping the algorithm from adding it again
member_of_rest(_,[]) :- false.
member_of_rest(arc(C,p(X,Y)), [[arc(C1, p(X1,Y1))|T]|T1]) :-
    X == X1, Y == Y1, C1 =< C
    ; 
    member_of_rest(arc(C,p(X,Y)), T1).

% Check if node has already been visited, using the same checks as above
member_of_list(_, []) :- false.
member_of_list(arc(C,p(X,Y)), [arc(C1,p(X1,Y1))|T]) :- 
    X == X1, Y == Y1, C1 =< C
    ; 
    member_of_list(arc(C,p(X,Y)), T).