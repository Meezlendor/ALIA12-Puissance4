:-use_module("Valuation.pl")


chooseMove(Board,Move,o):-minimax(3,Board,-1,Move,_).

getPlayer(1,'o'). % Identification du joueur selon le flag minmax/maxmin
getPlayer(-1,'x').

value(Board, Move, o, V):-Player='o',
    positionHorizontale(Player,Move,Board,V1),!,positionVerticale(Player,Move,Board,V2),!,positionDiagonale(Player,Move,Board,V3),!,
    V is V1+V2+V3.
    
value(Board, Move, x, V):-Player='x',
   positionHorizontale(Player,Move,Board,V1),!,positionVerticale(Player,Move,Board,V2),!,positionDiagonale(Player,Move,Board,V3),!,
    V is V1+V2+V3.

value(Board,5000):-
    not((not(positionHorizontale(_,_,Board,1000)),not(positionVerticale(_,_,Board,1000)),not(positionDiagonale(_,_,Board,1000)))).
value(Board,V):-
    findall(V,(member(Move,[0,1,2,3,4,5,6]),value(Board,Move,o,V)),Values1),
    sommeListe(Values1,Vo),
    findall(V,(member(Move,[0,1,2,3,4,5,6]),value(Board,Move,x,V)),Values2),
    sommeListe(Values2,Vx),
    V is Vo+Vx*5.
sommeListe([X],X).
sommeListe([X|L],V):-sommeListe(L,V2), V is V2+X.

%Algo minmax

minimax(0,Board, Flag,_,Value):-
    value(Board,Val),
    Value is Flag*Val.
minimax(Depth, Board, Flag, Move, Value):-
    Depth>0,
    playMove(Moves, Board), %define move
    DepthRecur is Depth-1,
    OtherFlag is -1*Flag,
    evaluateAndChoose(Moves, Board, DepthRecur, OtherFlag, (nil, -100000), (Move,Value)).

evaluateAndChoose([],_,_,_,Record,Record).
evaluateAndChoose([Move|Moves], Board, Depth,Flag, Record, BestMoves):-
    getPlayer(Flag,Player),
    playMove(Board,Move, NewBoard,Player),
    minimax(Depth, NewBoard, Flag, _,Value1),%MoveX is useless, we don't need to know what to do afterwards
    Value is -Value1,
    %write("Profondeur : "),write(Depth),write(" "),write(Player), write(" valeur : "), write(Value), write("("),write(Move),writeln(")"),
    update(Move,Value,Record,Record1),
    evaluateAndChoose(Moves,Board,Depth,Flag,Record1,BestMoves).

update(_, Value, (Move1, Value1), (Move1,Value1)):-Value=<Value1.
update(Move, Value, (_, Value1), (Move, Value)):-Value > Value1.