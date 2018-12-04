test :- testAjoutPiece, testWinHorizontale, testWinVerticale, testWinDiagonale.

testWinHorizontale :- testWinHoriz1, testWinHoriz2, testWinHoriz3.
testWinHoriz1 :- length(Board,42),assert(board(Board)),
    			nth0(28,Board,x),nth0(29,Board,x),nth0(30,Board,x),nth0(31,Board,x),
    	   		winHoriz(x,Board).
testWinHoriz2 :- length(Board,42),assert(board(Board)),
    			nth0(28,Board,x),nth0(29,Board,x),nth0(30,Board,o),nth0(31,Board,x),
    	   		not(winHoriz(x,Board)). 
testWinHoriz3 :- length(Board,42),assert(board(Board)),
    			nth0(27,Board,x),nth0(28,Board,x),nth0(29,Board,x),nth0(30,Board,x),
    	   		not(winHoriz(x,Board)).  

testWinVerticale :- testWinVert1, testWinVert2, testWinVert3.
testWinVert1 :- length(Board,42),assert(board(Board)),
    			nth0(15,Board,x),nth0(22,Board,x),nth0(29,Board,x),nth0(36,Board,x),
    	   		winVertic(x,Board).
testWinVert2 :- length(Board,42),assert(board(Board)),
    			nth0(15,Board,x),nth0(22,Board,x),nth0(29,Board,o),nth0(36,Board,x),
    	   		not(winVertic(x,Board)). 
testWinVert3 :- length(Board,42),assert(board(Board)),
    			nth0(15,Board,x),nth0(22,Board,x),nth0(29,Board,x),nth0(36,Board,o),nth0(8,Board,x),
    	   		winVertic(x,Board).

testWinDiagonale :- testWinDiag1, testWinDiag2, testWinDiag3.
testWinDiag1 :- length(Board,42),assert(board(Board)),
    			nth0(15,Board,x),nth0(23,Board,x),nth0(31,Board,x),nth0(39,Board,x),
    	   		winDiag(x,Board). %diagonale vers gauche
testWinDiag2 :- length(Board,42),assert(board(Board)),
    			nth0(18,Board,x),nth0(24,Board,x),nth0(30,Board,x),nth0(36,Board,x),
    	   		winDiag(x,Board). %diagonale vers droite
testWinDiag3 :- length(Board,42),assert(board(Board)),
    			nth0(18,Board,x),nth0(24,Board,o),nth0(30,Board,x),nth0(36,Board,x),
    	   		not(winDiag(x,Board)). 

winHoriz(Player,Board) :- board(Board), positionHorizontale(Player,_,Board,1000),!.
winVertic(Player,Board) :- board(Board), positionVerticale(Player,_,Board,1000),!.
winDiag(Player,Board) :- board(Board), positionDiagonale(Player,_,Board,1000),!.

testAjoutPiece:- testAjoutPieceCol(0), testAjoutPieceCol(1), 
    			  testAjoutPieceCol(2), testAjoutPieceCol(3),
    			  testAjoutPieceCol(4), testAjoutPieceCol(5),
    			  testAjoutPieceCol(6), not(testAjoutPieceCol(7)),
    			  not(testAjoutPieceCol(-1)),!.
testAjoutPieceCol(Col):- length(Board,42), assert(board(Board)),playMove(Board,Col,_,_).

testHauteurJeton :- testHauteurJeton1, testHauteurJeton2, testHauteurJeton3.
testHauteurJeton1 :- length(Board,42),assert(board(Board)),
                     nth0(25,Board,x),nth0(32,Board,x),nth0(39,Board,x),
                     hauteurJeton(4,Board,H),H is 3.
testHauteurJeton2 :- length(Board,42),assert(board(Board)),
                     nth0(3,Board,x),
                     hauteurJeton(3,Board,H),H is 0.
testHauteurJeton3 :- length(Board,42),assert(board(Board)),
                     hauteurJeton(4,Board,H),H is 6.
					 
					 
%test
length(Board,42),nth0(3,Board,x),nth0(7,Board,x),nth0(9,Board,x),nth0(15,Board,x),nth0(21,Board,o),nth0(23,Board,o),nth0(31,Board,x),nth0(39,Board,x),blocageDiagonale(x,1,Board,V).

%Test distance Verticale :
length(A,42),nth0(7,A,x),nth0(14,A,x),nth0(21,A,o),nth0(28,A,x),nth0(35,A,x),distanceVerticale(x,0,A,V).
%Test de valuation :
length(A,7),nth0(0,A,x),nth0(1,A,x),nth0(2,A,x),nth0(3,A,x),nth0(4,A,x),nth0(5,A,x),nth0(6,A,x),evaluerDistanceLigne(x,6,A,V).
%Test distance :
length(A,7),nth0(0,A,x),nth0(1,A,x),nth0(2,A,x),nth0(3,A,x),nth0(4,A,x),nth0(5,A,x),nth0(6,A,x),trace,evaluerDistance(x,0,A,V).
%Test de valuation :
length(A,7),nth0(0,A,x),nth0(1,A,x),nth0(2,A,x),nth0(3,A,x),nth0(4,A,x),nth0(5,A,x),nth0(6,A,x),evaluerDistanceLigne(x,6,A,V).

%Test diagonale
length(Board,42),nth0(21,Board,x),nth0(15,Board,x),nth0(9,Board,x),nth0(3,Board,x),nth0(29,Board,x),nth0(37,Board,x),positionDiagonale(x,0,Board,V). %V=1000
length(Board,42),nth0(22,Board,x),nth0(14,Board,x),nth0(30,Board,x),nth0(38,Board,x),nth0(16,Board,x),nth0(10,Board,x),nth0(4,Board,x),positionDiagonale(x,1,Board,V). %V=2000
length(Board,42),nth0(23,Board,x),nth0(17,Board,x),nth0(15,Board,x),nth0(31,Board,x),positionDiagonale(x,2,Board,V).%V=110
length(Board,42),nth0(24,Board,x),nth0(30,Board,x),nth0(32,Board,x),trace,positionDiagonale(x,3,Board,V).%V=20
length(Board,42),nth0(18,Board,x),nth0(24,Board,x),nth0(12,Board,x),nth0(6,Board,x),nth0(10,Board,x),positionDiagonale(x,4,Board,V). %V=1010
length(Board,42),nth0(19,Board,x),nth0(11,Board,x),nth0(3,Board,x),trace,positionDiagonale(x,5,Board,V).%V=100
length(Board,42),nth0(27,Board,x),nth0(19,Board,x),nth0(11,Board,x),trace,positionDiagonale(x,6,Board,V).%V=100
%Test vertical
length(Board,35),nth0(0,Board,x),nth0(7,Board,x),nth0(14,Board,x),nth0(21,Board,o),nth0(28,Board,x),trace,positionVerticale(x,M,Board,V).
% Test positionHorizontale
length(Board,35),nth0(28,Board,x),nth0(29,Board,x),nth0(30,Board,x),nth0(31,Board,x),trace,positionHorizontale(x,M,Board,V).