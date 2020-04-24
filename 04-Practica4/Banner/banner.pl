%% A factory produces banners using only a set of existing rectangular
%% pieces.  Our goal is to find out how to use the minimum set of
%% pieces that exactly match the design of the banner. Note that
%% pieces can be rotated if necessary. Also, note that each piece can
%% be used at most once.  That's why there can be several identical
%% pieces in the input.

%%%%%%%%%%%%%%%%%%%%% INPUT EXAMPLE:

%% banner( [                             the x's define the design of the banner
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,x,x,x,x,x,x,.],
%% 	       [.,x,x,x,x,x,x,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.],
%% 	       [.,x,x,x,.,.,.,x,x,x,.]
%% 	   ]).
%% pieces([
%% 	    [1,3,8],   % piece 1 is a 3 x 8 rectangle
%% 	    [2,3,3],   % piece 2 is a 3 x 3 rectangle
%% 	    [3,9,2],   % ...
%% 	    [4,3,3],
%% 	    [5,3,8],
%% 	    [6,3,2],
%% 	    [7,3,3],
%% 	    [8,3,3],
%% 	    [9,2,3],
%% 	    [a,1,3]	    
%% 	  ]).

%% A possible solution using 6 pieces:
%% .444...888.
%% .444...888.
%% .444...888.
%% .333333333.
%% .333333333.
%% .a99...777.
%% .a99...777.
%% .a99...777.

%% An optimal solution using 3 pieces:
%% .555...111.
%% .555...111.
%% .555...111.
%% .555666111.
%% .555666111.
%% .555...111.
%% .555...111.
%% .555...111.
      	 

%%%%%% Some helpful definitions to make the code cleaner:
piece(P):-                  pieces(L), member([P,_,_],L).
pieceSize(P,W,H):-          pieces(L), member([P,W,H],L).
widthBanner(W):-            banner(B), member(L,B), length(L,W),!.
heightBanner(H):-           banner(B), length(B,H), !.
contentsCellBanner(X,Y,C):- cell(X,Y), banner(B), heightBanner(H), Y1 is H-Y+1, nth1(Y1,B,L), nth1(X,L,C).
cell(X,Y):-                 widthBanner(W), heightBanner(H), between(1,W,X), between(1,H,Y).
%% Hay que usar mas definiciones para facilitar la redaccion de las restricciones.

% You can use the following types of symbolic propositional variables:
%   1. pieceCell(P,X,Y) means:   "piece P fills cell [X,Y]" (note: [1,1] is the bottom-left cell of the banner (Careful: Mandatory variable. Otherwise, displaySol will not work)
%   2. rotated(P) means:         "piece P is rotated"
%   3. pieceStarts(P,X,Y) means: "bottom-left cell of piece P is in cell [X,Y]"
%   4. used(P) means:            "piece P is used"


%%%%%%%%%%%%%%%%%

writeClauses(infinite):- !, writeClauses(N),!.
writeClauses(MaxPieces):-
    %% cap pesa esta situada en una posicio que fa que surti fora del banner (fa que su X,Y, invalids), hay que contemplar si esta rotada o no
    %% una pesa nomes (at most) en una posicio
    %% ...
    %% una pesa no por estar sobre una altre
    %% una pesa encaixa dintre dels x del banner (do not fill points)
    %% ...
    atMostUsed(K),  %% No se usan mas de K piezas


%%%%%%%%%%%%%%%%%

displaySol(M):-
    widthBanner(W),
    heightBanner(H),
    between(1,H,YB),
    nl,
    Y is H-YB+1,
    between(1,W,X),
    writeCell(M,X,Y),
    fail.
displaySol(_):-nl.

writeCell(M,X,Y):- member(pieceCell-P-X-Y,M), !, write(P).
writeCell(_,_,_):- write('.').

%% JESUCRISTO HA NACIDO: writeClause([-a,b]) === a => b