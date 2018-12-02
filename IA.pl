%--------------------------------------Algorithme min max

getPlayer(1,'o'). % Identification du joueur selon le flag minmax/maxmin
getPlayer(-1,'x').

%%Valuation du plateau selon le joueur,

value(Board, Move, o, V):-Player='o',
    positionHorizontale(Player,Move,Board,V1),positionVerticale(Player,Move,Board,V2),positionDiagonale(Player,Move,Board,V3),
   distanceHorizontale(Player,Move,Board,V4),distanceVerticale(Player,Move,Board,V5),distanceDiagonale(Player,Move,Board,V6),
    V is V1+V2+V3+V4+V5+V6.
    
value(Board, Move, x, V):-Player='x',
   positionHorizontale(Player,Move,Board,V1),positionVerticale(Player,Move,Board,V2),positionDiagonale(Player,Move,Board,V3),
    V is (V1+V2+V3)/2.

value(Board,V):-
    findall(V,(member(Move,[0,1,2,3,4,5,6]),value(Board,Move,o,V)),Values1),
    sommeListe(Values1,Vo),
    findall(V,(member(Move,[0,1,2,3,4,5,6]),value(Board,Move,x,V)),Values2),
    sommeListe(Values2,Vx),
    V is Vo+Vx.
sommeListe([X],X).
sommeListe([X|L],V):-sommeListe(L,V2), V is V2+X.

%Algo minmax

minimax(0,Board, _,_,Value):-
    value(Board,Value).
minimax(Depth, Board, Flag, Move, Value):-
    Depth>0,
    playMove(Moves, Board), %define move
    DepthRecur is Depth-1,
    OtherFlag is -1*Flag,
    evaluateAndChoose(Moves, Board, DepthRecur, OtherFlag, (nil, -10000), (Move,Value)).

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

%Algo alphabeta

alphaBeta(0,Board,_,_,_,_,Value):-value(Board,Value).
alphaBeta(D,Board,Alpha,Beta,Flag, Move,Value):-
    playMove(Moves,Board),
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    D1 is D-1,
    Flag1 is -Flag,
    evaluateAndChoose(Moves,Board,D1,Alpha1,Beta1,Flag1,nil, (Move,Value)).

evaluateAndChoose([],_,_,Alpha,_,_,Move,(Move,Alpha)).
evaluateAndChoose([Move|Moves], Board, D, Alpha, Beta, Flag,Move1, BestMove):-
    getPlayer(Flag,Player),
    playMove(Board,Move,NewBoard,Player),
    alphaBeta(D,NewBoard,Alpha,Beta,Flag,_,Value1),
    Value is -Value1,
    cutoff(Move,Value,D,Alpha,Beta,Flag,Moves,Board,Move1,BestMove).

cutoff(Move,Value,_,_,Beta,_,_,_,_,(Move,Value)):-
    Value>=Beta.
cutoff(Move,Value,D,Alpha,Beta,Flag,Moves,Board,_,BestMove):-
    Alpha < Value, Value < Beta,
    evaluateAndChoose(Moves,Board,D,Value,Beta,Flag,Move,BestMove).
cutoff(_,Value,D,Alpha,Beta,Flag,Moves,Board,Move1,BestMove):-
    Value=<Alpha,
    evaluateAndChoose(Moves,Board,D,Alpha,Beta,Flag,Move1,BestMove).