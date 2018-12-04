%--------------------Fonction d'évaluation d'une ligne

valeurDeV(V,V):-V\=1.
valeurDeV(1,0).
evaluer(Player,[X|L], Value):-X==Player,not(var(X)),evaluer(Player,L,V), Value is 10*V.
evaluer(Player,[X|_],1):-X\==Player.
evaluer(_,[],1).
evaluerLigne(_,_,[],0).
evaluerLigne(Player,M,L,0):-not(nth0(M,L,Player)).
evaluerLigne(Player,M,L,Value):-nth0(M,L,Player),separerLigne(M,L,LG,LD), reverse(LG,GL), evaluer(Player,GL,V2),evaluer(Player,LD,V3), V is V2*V3, valeurDeV(V,Value).
construireGauche(M,M,_,[]).
construireGauche(M,M2,[X|L],LG):- M3 is M2+1,construireGauche(M,M3,L,LG2),append([X],LG2,LG).
construireDroite(_,_,[],[]).
construireDroite(M,M2,[X|L],LD):-M2>M,M3 is M2+1, construireDroite(M,M3,L,LD2), append([X],LD2,LD).
construireDroite(M,M2,[_|L],LD):-M2=<M,M3 is M2+1,construireDroite(M,M3,L,LD).
separerLigne(M, L, LG, LD):-construireGauche(M,0,L,LG),construireDroite(M,0,L,LD).

%--------------------Analyse du tableau de jeu

%ligne horizontale

positionHorizontale(_,_,[],0).
positionHorizontale(Player, M, [X0,X1,X2,X3,X4,X5,X6|_],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val), Val==Player,not(var(Val)),evaluerLigne(Player,M,L,V).
positionHorizontale(Player, M, [X0,X1,X2,X3,X4,X5,X6|Board],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),Val\==Player,positionHorizontale(Player, M, Board,V).


  
%ligne verticale

construireVerticale(_,[],[]).
construireVerticale(M,[X0,X1,X2,X3,X4,X5,X6|Board],C):-nth0(M,[X0,X1,X2,X3,X4,X5,X6],Val),construireVerticale(M,Board,C2), append([Val],C2,C).
evaluerVerticale(Player,[X|_],0):-not(var(X)),X\==Player.
evaluerVerticale(Player,[X|C],V):-not(var(X)),X==Player,evaluer(Player,C,V).
evaluerVerticale(Player,[X|C],V):-var(X),evaluerVerticale(Player,C,V).
positionVerticale(_,M,Board,0):-hauteurJeton(M,Board,6).
positionVerticale(Player,M,Board,V):-construireVerticale(M,Board,Verticale),evaluerVerticale(Player,Verticale,V).



%ligne diagonale
%%voc : diagonale gauche = qui descend vers la gauche, diagonale droite = qui descend vers la droite

construireDiagonaleG(_,[],[]).
construireDiagonaleG(H,[_|_],[]):-H<0.
construireDiagonaleG(H,[_|_],[]):-H>8.
construireDiagonaleG(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-not(nth0(H,[X0,X1,X2,X3,X4,X5,X6],_)),H>6,H<9,H2 is H-1,construireDiagonaleG(H2,Board,Diag2),
    append([X],Diag2,Diag).
construireDiagonaleG(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-nth0(H,[X0,X1,X2,X3,X4,X5,X6],Val), H2 is H-1,construireDiagonaleG(H2,Board,Diag2),
    append([Val],Diag2,Diag).
construireDiagonaleD(_,[],[]).
construireDiagonaleD(H,[_|_],[]):-H>6.
construireDiagonaleD(H,[_|_],[]):- -2>H.
construireDiagonaleD(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-not(nth0(H,[X0,X1,X2,X3,X4,X5,X6],_)), H<0,-3<H,H2 is H+1, construireDiagonaleD(H2,Board,Diag2),
    append([X],Diag2,Diag).
construireDiagonaleD(H,[X0,X1,X2,X3,X4,X5,X6|Board],Diag):-nth0(H,[X0,X1,X2,X3,X4,X5,X6],Val), H2 is H+1,construireDiagonaleD(H2,Board,Diag2),
    append([Val],Diag2,Diag).

hauteurJeton(_,[],0).
hauteurJeton(M,[X0,X1,X2,X3,X4,X5,X6|_],0):-nth0(M,[X0,X1,X2,X3,X4,X5,X6],Val),not(var(Val)).
hauteurJeton(M,[X0,X1,X2,X3,X4,X5,X6|Board],H):-nth0(M,[X0,X1,X2,X3,X4,X5,X6|Board],Val),var(Val),hauteurJeton(M,Board,H2),H is H2+1.

indexDiagG(M,H,IC):-IC is M+H.
indexDiagD(M,H,IC):-IC is M-H.

positionDiagonale(Player,M,Board,V):-hauteurJeton(M,Board,H),
    indexDiagG(M,H,ICG),construireDiagonaleG(ICG,Board,DiagG),evaluerLigne(Player,H,DiagG,VG),
    indexDiagD(M,H,ICD),construireDiagonaleD(ICD,Board,DiagD),evaluerLigne(Player,H,DiagD,VD),
    V is VD+VG.



%-----------------------------------Valuations des distances

valeurDistance(2,20).
valeurDistance(3,10).
valeurDistance(4,5).
valeurDistance(X,0):-X<2.
valeurDistance(X,0):-X>4.
evaluerDistance(_,_,[],0).
evaluerDistance(Player,D,[X|L],V):-D<4,not(var(X)),X==Player,D2 is D+1,evaluerDistance(Player,D2,L,V2),valeurDistance(D,V3), V is V2+V3.
evaluerDistance(Player,D,[X|L],V):-D<4,(var(X);not(var(X)),X\==Player),D2 is D+1, evaluerDistance(Player,D2,L,V).
evaluerDistance(_,4,_,0).
evaluerDistanceLigne(_,_,[],0).
evaluerDistanceLigne(Player,M,L,V):-separerLigne(M,L,LG,LD),reverse(LG,GL),evaluerDistance(Player,1,GL,VG),evaluerDistance(Player,1,LD,VD), V is VG +VD.

%-----------------------------Analyse des distances dans le tableau
%ligne horizontale
distanceHorizontale(Player,M,[X0,X1,X2,X3,X4,X5,X6|_],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),not(var(Val)),evaluerDistanceLigne(Player,M,L,V).
distanceHorizontale(Player, M, [X0,X1,X2,X3,X4,X5,X6|Board],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val),var(Val), distanceHorizontale(Player,M,Board,V).


%ligne verticale
reduireVerticale([X|L],[X|L]):-not(var(X)).
reduireVerticale([X|L],Verticale):-var(X),reduireVerticale(L,Verticale).
distanceVerticale(Player,M,Board,V):-construireVerticale(M,Board,Col),reduireVerticale(Col,Verticale),evaluerDistance(Player,0,Verticale,V).


%ligne diagonale
evaluerDistanceDiagonaleG(Player,M,Board,V):-hauteurJeton(M,Board,H),indexDiagG(M,H,ICG),construireDiagonaleG(ICG,Board,Diag),evaluerDistanceLigne(Player,H,Diag,V).
evaluerDistanceDiagonaleD(Player,M,Board,V):-hauteurJeton(M,Board,H),indexDiagD(M,H,ICD),construireDiagonaleD(ICD,Board,Diag),evaluerDistanceLigne(Player,H,Diag,V).

distanceDiagonale(Player,M,Board,V):-evaluerDistanceDiagonaleG(Player,M,Board,VG),evaluerDistanceDiagonaleD(Player,M,Board,VD),V is VG+VD.

%-----------------------------------Analyse du blocage
%
%evaluerBlocage(Player,[X,Y|_],5):-X\==Player,Y\==X,not(var(X)).
%evaluerBlocage(Player,[X],5):-X\==Player,not(var(X)).
%evaluerBlocage(Player,[X|Board],V):-X\==Player,not(var(X)),evaluerBlocage(Player,Board,V2),V is V2*10,V=<500,V>5.
%evaluerBlocage(Player,[Player|_],0).
%evaluerBlocage(_,[],0).
%evaluerBlocage(_,[X,X,X,X|_],0):-not(var(X)).

%evaluerBlocageLigne(Player,M,L,V):-separerLigne(M,L,LG,LD),reverse(LG,GL),evaluerBlocage(Player,GL,VG),evaluerBlocage(Player,LD,VD),V is VG +VD.
%evaluerBlocageLigne(_,_,[],0).

%blocageHorizontale(Player, M, [X0,X1,X2,X3,X4,X5,X6|_],V):-L=[X0,X1,X2,X3,X4,X5,X6],nth0(M,L,Val), not(var(Val)),Val==Player,evaluerBlocageLigne(Player,M,L,V).
%blocageHorizontale(Player, M, [_,_,_,_,_,_,_|Board],V):-blocageHorizontale(Player, M, Board,V).

%blocageVerticale(Player,M,Board,V):-construireVerticale(M,Board,Col),evaluerBlocage(Player,Col,V).
%evaluerBlocageDiagonaleG(Player,M,Board,V):-hauteurJeton(M,Board,H),indexDiagG(M,H,ICG),construireDiagonaleG(ICG,Board,Diag),evaluerBlocageLigne(Player,H,Diag,V).
%evaluerBlocageDiagonaleD(Player,M,Board,V):-hauteurJeton(M,Board,H),indexDiagD(M,H,ICD),construireDiagonaleD(ICD,Board,Diag),evaluerBlocageLigne(Player,H,Diag,V).

%blocageDiagonale(Player,M,Board,V):-evaluerBlocageDiagonaleG(Player,M,Board,VG),evaluerBlocageDiagonaleD(Player,M,Board,VD),V is VG+VD.