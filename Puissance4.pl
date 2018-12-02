:- dynamic board/1.
:-use_module("IA.pl")
:-use_module("Valuation.pl")
%-------------------------------------Condition de victoire

win(Player) :- board(Board), positionHorizontale(Player,_,Board,1000),!.
win(Player) :- board(Board), positionVerticale(Player,_,Board,1000),!.
win(Player) :- board(Board), positionDiagonale(Player,_,Board,1000),!.
gameOver(o) :- win(o).
gameOver(x):-win(x).
gameOver("Draw"):-board(Board),playMove([],Board).


play(_):-gameOver(Winner),!,displayBoard,writeln("Fin de la partie, victoire : "),writeln(Winner).
play(Player):- write('New turn for:'), writeln(Player),
        board(Board), % instanciate the board from the knowledge base
        displayBoard, % print it
            chooseMove(Board, Move,Player), % ask the AI for a move, that is, an index for the Player
        playMove(Board,Move,NewBoard,Player), % Play the move and get the result in a new Board
            applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
        changePlayer(Player,NextPlayer), % Change the player before next turn
            play(NextPlayer). % next turn


%Pour tester, on joue x et o :
chooseMove(_, Move, x) :-writeln("Entrez l'indice de la colonne dans laquelle vous souhaitez jouer :"),read(M2), Move is M2-1.
%chooseMove(_, Move, Player) :-writeln("Entrez l'indice de la colonne dans laquelle vous souhaitez jouer :"),read(M2), Move is M2-1.
%% Ligne au dessus à commenter/dé-commenter

%chooseMove(Board,Move,o).%:-repeat, Move is random(6), nth0(Move, Board, Elem), var(Elem), !.
%chooseMove(Board,Move,o):-minimax(3,Board,-1,Move,_).
chooseMove(Board,Move,o):-alphaBeta(4,Board,-10000,10000, -1,Move,_).
positionInBoard(_,[],Move,Move).
positionInBoard(_,Board,Move,Move) :-nth0(Move,Board,Val),not(var(Val)),not(nth0(Move,Board,?)),!.
positionInBoard(Player,[_,_,_,_,_,_,_|Board],Move,Index):-positionInBoard(Player,Board,Move,I2),Index is I2+7.

buildNewBoard([_|Board],0,[Player|Board],Player).
buildNewBoard([X|Board],I,NewBoard,Player):-I\==0, I2 is I-1, buildNewBoard(Board,I2,NewBoard2,Player),append([X],NewBoard2,NewBoard).

%%%% Play a Move, the new Board will be the same, but one value will be instanciated with the Move
playMove(Board,Move,NewBoard,Player) :-
    positionInBoard(Player,Board,Move,I2),
    Index is I2-7,
    Index>=0,
    buildNewBoard(Board,Index,NewBoard,Player).

playMove(Moves,[X0,X1,X2,X3,X4,X5,X6|_]) :-findall(I,(nth0(I,[X0,X1,X2,X3,X4,X5,X6],variable)),Moves).

%%%% Remove old board/save new on in the knowledge base
applyIt(Board,NewBoard) :- retract(board(Board)), assert(board(NewBoard)).

%%%% Predicate to get the next player
changePlayer('x','o').
changePlayer('o','x').

%%%% Print the value of the board at index N:
% if its a variable, print ? and x or o otherwise.
printVal(N) :- board(B), nth0(N,B,Val), var(Val), write('•'), !.
printVal(N) :- board(B), nth0(N,B,Val), write(Val).

%%%% Display the board
displayBoard:-
    writeln('*-------------------------*'),
    printVal(0), write('  '), printVal(1), write('  '), printVal(2), write('  '), printVal(3), write('  '), printVal(4), write('  '), printVal(5), write('  '), printVal(6), writeln('  '),
    printVal(7), write('  '), printVal(8),write('  '), printVal(9), write('  '), printVal(10), write('  '), printVal(11),write('  '), printVal(12), write('  '), printVal(13), writeln(''),
    printVal(14), write('  '), printVal(15),write('  '), printVal(16),write('  '),  printVal(17),write('  '),  printVal(18),write('  '), printVal(19), write('  '), printVal(20), writeln(''),
    printVal(21), write('  '), printVal(22),write('  '), printVal(23),write('  '),  printVal(24),write('  '),  printVal(25),write('  '), printVal(26), write('  '), printVal(27), writeln(''),
    printVal(28), write('  '), printVal(29),write('  '), printVal(30),write('  '),  printVal(31), write('  '), printVal(32),write('  '), printVal(33), write('  '), printVal(34), writeln(''),
    printVal(35), write('  '), printVal(36),write('  '), printVal(37), write('  '), printVal(38),write('  '),  printVal(39),write('  '), printVal(40), write('  '), printVal(41), writeln(''),
    writeln('1 2 3 4 5 6 7'),
    writeln('*-------------------------*').

%%%%% Start the game!
init :- length(Board,42), assert(board(Board)), play('x'),!.