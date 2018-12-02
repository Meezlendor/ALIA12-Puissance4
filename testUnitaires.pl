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
