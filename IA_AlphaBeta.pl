:-use_module("Valuation.pl")

chooseMove(Board,Move,o):-alphaBeta(4,Board,-10000,10000, -1,Move,_).

getPlayer(1,'o'). % Identification du joueur selon le flag minmax/maxmin
getPlayer(-1,'x').

value(Board, Move, o, V):-Player='o',
    positionHorizontale(Player,Move,Board,V1),positionVerticale(Player,Move,Board,V2),positionDiagonale(Player,Move,Board,V3),
   distanceHorizontale(Player,Move,Board,V4),distanceVerticale(Player,Move,Board,V5),distanceDiagonale(Player,Move,Board,V6),
    V is V1+V2+V3+V4+V5+V6.
    
value(Board, Move, x, V):-Player='x',
   positionHorizontale(Player,Move,Board,V1),positionVerticale(Player,Move,Board,V2),positionDiagonale(Player,Move,Board,V3),
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

%Algo alphabeta

alphaBeta(0,Board,_,_,Flag,_,Value):-value(Board,Val), Value is Val*Flag.
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