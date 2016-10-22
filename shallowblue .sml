

(*
DATATYPE REPRENSENTATION: typeofpiece represents each of the chess pieces that exists in the game of chess. It also represents empty squares on the board as none.
DATATYPE CONVENTION: true
*)


datatype typeofpiece = Typeofpiece of int | Pawn | Rook | Knight | Queen| King | Bishop | none

(*
DATATYPE REPRENSENTATION: The colors which the pieces can have (the suit). Blanc is used for empty squares on the board.
DATATYPE CONVENTION: true

*)
datatype suit = Suit of int | Black | White | Blanc

(*
DATATYPE REPRENSENTATION: The chess pieces as typeofpiece and suit combined.
DATATYPE CONVENTION: true

*)
 
datatype piece = Piece of (suit * typeofpiece) | Empty


exception Nodirection
exception NoPiece

(* getBoard()
   TYPE: unit ->(piece * (int * int)) list list
   PRE: true
   POST: The starting board
*)

fun getBoard() =
    let 
	val row1 = [(Piece(White, Rook),(0,0)),(Piece(White, Knight),(1,0)),(Piece(White, Bishop),(2,0)),
		     (Piece(White, Queen),(3,0)),(Piece(White, King),(4,0)),
		     (Piece(White, Bishop),(5,0)),(Piece(White, Knight),(6,0)),(Piece(White, Rook),(7,0))]

	val row2 = [(Piece(White, Pawn),(0,1)),(Piece(White, Pawn),(1,1)),(Piece(White, Pawn),(2,1)),
		    (Piece(White, Pawn),(3,1)),(Piece(White, Pawn),(4,1)),
		    (Piece(White, Pawn),(5,1)),(Piece(White, Pawn),(6,1)),(Piece(White, Pawn),(7,1))]

	val row3 = [((Empty),(0,2)),((Empty),(1,2)),((Empty),(2,2)),((Empty),(3,2)),((Empty),(4,2)),
		    ((Empty),(5,2)),((Empty),(6,2)),((Empty),(7,2))]

	val row4 = [((Empty),(0,3)),((Empty),(1,3)),((Empty),(2,3)),((Empty),(3,3)),((Empty),(4,3)),
		   ((Empty),(5,3)),((Empty),(6,3)),((Empty),(7,3))]

	val row5 = [((Empty),(0,4)),((Empty),(1,4)),((Empty),(2,4)),((Empty),(3,4)),((Empty),(4,4)),
		   ((Empty),(5,4)),((Empty),(6,4)),((Empty),(7,4))]

	val row6 = [((Empty),(0,5)),((Empty),(1,5)),((Empty),(2,5)),((Empty),(3,5)),((Empty),(4,5)),
		    ((Empty),(5,5)),((Empty),(6,5)),((Empty),(7,5))]

	val row7 = [(Piece(Black, Pawn),(0,6)),(Piece(Black, Pawn),(1,6)),(Piece(Black, Pawn),(2,6)),
		    (Piece(Black, Pawn),(3,6)),(Piece(Black, Pawn),(4,6)),
		    (Piece(Black, Pawn),(5,6)),(Piece(Black, Pawn),(6,6)),(Piece(Black, Pawn),(7,6))]

	val row8 = [(Piece(Black, Rook),(0,7)),(Piece(Black, Knight),(1,7)),(Piece(Black, Bishop),(2,7)),
		    (Piece(Black, Queen),(3,7)),(Piece(Black, King),(4,7)),
		    (Piece(Black, Bishop),(5,7)),(Piece(Black, Knight),(6,7)),(Piece(Black, Rook),(7,7))]


 	val board = [row1,row2,row3,row4,row5,row6,row7,row8]
    in
	board 
    end


(* replaceWith l,i,x
   TYPE: ''a list * ''a * ''a -> ''a list
   PRE: i is an element of board.
   POST: i replaced with x in l
   EXAMPLE: replaceWith ([1,2,3,4], 2,3) = [1,3,3,4]
   VARIANT: l

*)

fun replaceWith([],i,x) = []
  |replaceWith(b::bs,i,x) =
   if b = i then x::replaceWith(bs,i,x)
   else b::replaceWith(bs,i,x)



(*

  getSuit'(Piece * (int*int))
  TYPE: piece * 'a -> suit
  PRE:true
  POST: The Suit of a piece.
  EXAMPLE: getSuit'(Piece(Black, Rook),(0,3)) = Black: suit

*)


fun getSuit' (Empty,_) = Blanc
  |getSuit' ((Piece(Type as(a,b))),_) = a


(*

  getSuit (board,(x,y))
  TYPE: (piece * 'a) list list * (int * int) -> typeofpiece
  PRE: x, y are existing coordinates in board
  POST: The suit of a piece at position (x,y)
  EXAMPLE: getSuit(board, (1,1)) = White : suit

*)

fun getSuit (board ,(x,y)) = 
    let
	val requestedrow = List.nth(board,y) 
	val requestedPiece = (List.nth(requestedrow,x))
    in
	getSuit'(requestedPiece)
    end


(*
  
  getPiece'(Piece * (int*int))
  TYPE: piece * 'a -> suit
  PRE: true
  POST: The typeofpiece of Piece with the coordinates (int*int).
  EXAMPLE: getPiece'(Piece(White, Rook), (1,1)) = Rook : typeofpiece

*)


fun getPiece' (Empty,_) = none 
  |getPiece' ((Piece(Type as(a,b))),_) = b



(*

  getPiece (board,(x,y))
  TYPE: (piece * 'a) list list * (int * int) -> typeofpiece
  PRE: x, y are existing coordinates in board.
  POST: The typeofpiece of a piece at position (x,y).
  EXAMPLE: getPiece(board, (1,1)) = Pawn : typeofpiece


*)


fun getPiece (board ,(x,y)) = 
    let
	val requestedrow = List.nth(board,y) 
	val requestedPiece = (List.nth(requestedrow,x))
    in
	getPiece'(requestedPiece)
    end

(*getVerticalList (board,startrow,x,n,acc)
  TYPE: 'a list list * int * int * int * 'a list -> 'a list
  PRE: n is not of a higher value than the length of the list from x to the end of the list
       x is within the bounds of board
  POST: A list of the elements of the vertical path from the starting position x and the position n.
  EXAMPLE: getVerticalList(getBoard(), 3 , 6, 3 , []) = [(Empty, (6, 4)), (Empty, (6, 5)), (Piece (Black, Pawn), (6, 6))]
*)

fun getVerticalList (board,startrow,x,0,acc) = acc
  |getVerticalList (board,startrow,x,n,acc) = 
   getVerticalList(board,startrow,x,n-1, List.nth(List.nth(board,startrow+n),x)::acc)

(*
getDiagonalList (board,startrow,(Px, Py), (Tx, Ty),acc)
TYPE:  'a list list * ('b * 'c) * (int * int) * int * 'a list -> 'a list
PRE: Px >= Tx
POST: A list of n elements of the diagonal path from the starting position x in a startrow.
EXAMPLE: getDiagonalList(getBoard(),(0,0),(7,7),4,[]) = [(Empty, (4, 4)), (Empty, (5, 5)), (Piece (Black, Pawn), (6, 6))]
*)
fun getDiagonalList (board,PiecePos as (Px,Py),TargetPos as(Tx,Ty),0,acc) = tl(rev acc)
  |getDiagonalList(board,PiecePos as (Px,Py),TargetPos as(Tx,Ty),n,acc) = 
   getDiagonalList(board, PiecePos, TargetPos, n-1,(List.nth(List.nth(board,Ty-n),Tx-n)::acc))

(*
getDiagonalList2 (board,PiecePos,TargetPos,n,m,acc)
TYPE: a list list * (int * 'b) * ('c * int) * int * int * 'a list -> 'a list
PRE: PiecePos is the end position, targetPos is the starting position m=0 acc = []
POST: A list of n elements of the diagonal path between the two Points
EXAMPLE: getDiagonalList2(getBoard(),(7,0),(0,7),4,0,[]) = [(Empty, (6, 4)), (Empty, (5, 5)), (Piece (Black, Pawn), (4, 6))]
*)

fun getDiagonalList2 (board,PiecePos as (Px,Py),TargetPos as (Tx,Ty),0,m,acc) = tl(rev acc)
|getDiagonalList2 (board,PiecePos as (Px,Py),TargetPos as (Tx,Ty),n,m,acc) = 
 getDiagonalList2 (board,PiecePos,TargetPos,n-1,m+1,(List.nth(List.nth(board,Ty-n),Px-m)::acc))

(*
pathClear (board,(Px,Py),(Tx,Ty),direction)
TYPE:(piece * ''a) list list * (int * int) * (int * int) * int -> bool
PRE:(Px,Py) and (Tx,Ty) must be inside of the board. direction must correspond with the given coordinates ((Px,Py), (Tx,Ty)); ( If Px <> Tx and Ty = Py then direction must be 1. If Px = Tx and Py <> Ty then direction must be 2. If Px <> Tx and Py <> Ty then direction must be 3).
POST: Returns true if the path from (Px,Py) to (Tx,Ty) only contains elements of the typeofpiece none.

*)



fun pathClear (board,PiecePos as (Px,Py),TargetPos as (Tx,Ty),direction) =

    case direction of 
	1=>(*horizontal move*)
	if(Tx>Px) then
	    List.filter(fn x => getPiece'(x)<>none)(List.drop(List.take(List.nth(board,Py),Tx),Px+1)) = [] 
	else
	    List.filter(fn x => getPiece'(x)<>none)(List.drop(List.take(List.nth(board,Ty),Px),Tx+1)) = []
      |2=>(*vertical move*)
       if(Ty>Py) then
	   List.filter(fn x=> getPiece'(x)<>none) (getVerticalList(board,Py,Px,(Ty-Py)-1,[])) = []
       else
	   List.filter(fn x=> getPiece'(x)<>none) (getVerticalList(board,Ty,Px,(Py-Ty)-1,[])) = []
      |3=>(*diagonal move*)
       if(Tx>Px andalso Ty>Py) then(*right to left downward*)
	   List.filter(fn x=> getPiece'(x)<>none) (getDiagonalList(board,PiecePos,TargetPos,Ty-Py,[])) = []
       else if(Tx<Px andalso Ty>Py) then(*left to right downwardd*)
	   List.filter(fn x=> getPiece'(x)<>none) (getDiagonalList2(board,PiecePos,TargetPos,Ty-Py,0,[])) = []
       else if(Tx<Px andalso Ty<Py) then(*right to left upward*)
	   List.filter(fn x=> getPiece'(x)<>none) (getDiagonalList(board,TargetPos,PiecePos,Py-Ty,[])) = []
       else (*left ot right upward*)
	   List.filter(fn x=> getPiece'(x)<>none) (getDiagonalList2(board,TargetPos,PiecePos,Py-Ty,0,[])) = []

      |_=> raise 
	   Nodirection




(*

validMove(board,PiecePos as(Px,Py), TargetPos as (Tx,Ty),unit)
TYPE: (piece * ''a) list list * (int * int) * (int * int) * int -> bool
PRE: true
POST: true if the requested move is according to chess rules 
EXAMPLE:  validMove(getBoard(), (3,0), (7,1), 2) = false 
	  validMove(getBoard(), (7,1), (7,3), 1) = true
*)


fun validMove(board,PiecePos as(Px,Py), TargetPos as (Tx,Ty),unit) = 
    let
	val leSuit = getSuit(board,PiecePos)
    in
	if Tx<=7 andalso Tx>=0 andalso Ty>=0 andalso Ty<=7 then
	    case unit of
		1 => (*MovePawn*)
		if leSuit = White then
		    if Py = 1 andalso Ty = 3 andalso getPiece(board,(Tx,3)) = none andalso getPiece(board,(Tx,2)) = none andalso Px=Tx then 
			true
		    else if Py+1=Ty andalso getPiece(board,(Tx,Ty)) = none andalso Px=Tx then 
			true
		    else if Px+1=Tx andalso Ty=Py+1 andalso getSuit(board,(Tx,Ty)) = Black then 
			true
		    else if Px-1=Tx andalso Ty=Py+1 andalso getSuit(board,(Tx,Ty)) = Black then 
(*right to left upward*)			true
		    else 
			false
		else
		    if Py = 6 andalso Ty = 4 andalso getPiece(board,(Tx,4)) = none andalso getPiece(board,(Tx,5)) = none andalso Px=Tx then 
			true
		    else if Py-1=Ty andalso getPiece(board,TargetPos) = none andalso Tx=Px then
			true
		    else if Px+1=Tx andalso Ty=Py-1 andalso getSuit(board,TargetPos) = White then 
			true
		    else if Px-1=Tx andalso Ty=Py-1 andalso getSuit(board,TargetPos) = White then 
			true
		    else
			false

	      |2 => (*MoveRook*)
	       if Py=Ty andalso Px<>Tx then 
		   pathClear(board,PiecePos,TargetPos,1(*1 = horizontal move*))  andalso getSuit(board,TargetPos)<> leSuit
	       else if Px=Tx andalso Py<>Ty then 
		   pathClear(board,PiecePos,TargetPos,2(*2 = vertical move*)) andalso getSuit(board,TargetPos)<> leSuit
	       else 
		   false

	      |3 => (*MoveKnight*)
	       
	       if	Tx=Px+1 andalso Ty=Py+2 orelse Ty=Py-2 andalso getSuit(board,(Tx,Ty)) <> leSuit then 
		   true
	       else if Tx=Px-1 andalso Ty=Py+2 orelse Ty=Py-2 andalso getSuit(board,(Tx,Ty)) <> leSuit then
		   true
	       else if Tx=Px+2 andalso Ty=Py+1 orelse Ty=Py-1 andalso getSuit(board,(Tx,Ty)) <> leSuit then
		   true
	       else if Tx=Px-2 andalso Ty=Py+1 orelse Ty=Py-1 andalso getSuit(board,(Tx,Ty)) <> leSuit then
		   true
	       else 
		   false
		   
	      |4 =>(*Move Bishop*)
	       if Py<>Ty andalso Px<>Tx andalso pathClear(board,PiecePos,TargetPos,3(*3 = diagonal move*)) then
		   getSuit(board,TargetPos)<> leSuit
	       else
		   false
		   
	      |_ =>
	       raise NoPiece
	else 
	    false
    end



(*
replaceAt(list,i,x)
TYPE: 'a list * 'a * int -> 'a list
PRE: x=< length list
POST: An updated list with the element at position x replaced with i.
EXAMPLE: replaceAt([1,2,3,4,5],4,2) = [1, 2, 4, 4, 5]
*)



fun replaceAt(list,i,x) = 
	(List.take(list,x))@[i]@(List.drop(list,x+1))



(*
updateBoardVertical(board,(Px,Py),(Tx,Ty),unit)
TYPE:(piece * (int * int)) list list * (int * int) * (int * int) * piece -> (piece * (int * int)) list list
PRE: Px <> Tx, Py <> Ty
POST: Returns the updatded board with the unit who had (Px,Py) as coordinates with the new coordinates (Tx,Ty), leaving the postion (Px,Py) as Empty.
EXAMPLE: updateBoardVertical(getBoard(), (0,0), (1,1), Piece(Black, Pawn)) =
   [[(Empty, (0, 0)), (Piece (White, Knight), (1, 0)), (Piece (White, Bishop), (2, 0)), (Piece (White, Queen), (3, 0)),
     (Piece (White, King), (4, 0)), (Piece (White, Bishop), (5, 0)),
     (Piece (White, Knight), (6, 0)), (Piece (White, Rook), (7, 0))],
    [(Piece (White, Pawn), (0, 1)), (Piece (Black, Pawn), (1, 1)),
     (Piece (White, Pawn), (2, 1)), (Piece (White, Pawn), (3, 1)), .... ] .... ]

*)



fun updateBoardVertical(board,(Px,Py),(Tx,Ty),unit) =
    (replaceWith(replaceWith(board,List.nth(board,Py),List.take(List.nth(board,Py),Px)
    @
    [(Empty,(Px,Py))](*sets the old position to empty at PiecePos*)
    @
    List.drop(List.nth(board,Py),Px+1)),List.nth(board,Ty),List.take(List.nth(board,Ty),Tx)
    @
    [(unit,(Tx,Ty))](*sets the new position to the piece at TargetPos *)
    @
    List.drop(List.nth(board,Ty),Tx+1)))



(*
updateBoardHorizontal(board,(Px,Py),(Tx,Ty),unit)
TYPE: (piece * (int * int)) list list * (int * int) * (int * int) * piece ->  (piece * (int * int)) list list
PRE: Ty = Py, Px <> Tx
POST: The list in board containing the element at (Tx, Ty) where the element at (Tx, Ty) has been replaced with the element at (Px, Py) and the element at (Px, Py) is replaced with Empty.
EXAMPLE: updateBoardHorizontal(getBoard(), (0,0), (5,0), Piece(Black, Pawn)) =  [[(Empty, (0, 0)), (Piece (White, Knight), (1, 0)), (Piece (White, Bishop), (2, 0)), (Piece (White, Queen), (3, 0)), (Piece (White, King), (4, 0)), (Piece (Black, Pawn), (5, 0)), (Piece (White, Knight), (6, 0)), (Piece (White, Rook), (7, 0))] ..... ],
*)



fun updateBoardHorizontal(board,(Px,Py),(Tx,Ty),unit) = 
    let
	val oldrow = replaceAt(List.nth(board,Py),(Empty,(Px,Py)),Px)
	val newrow = replaceAt(oldrow,(unit,(Tx,Ty)),Tx)
	val beforerow = List.take(board,Py)
	val afterrow = List.drop(board,Py+1)
	val replaceold = beforerow@[newrow]@afterrow
    in
	replaceold
    end

(* getPos'(board, p, x,y)
	TYPE: (''a * (int * int)) list * ''a * int * int -> int * int
	PRE:  x,y <= 7, there is only one instance of p in board
	POST: the position in board of p
	EXAMPLE: getPos'(getBoard(),Piece(White,King), 0,0) = (4,0)
*)

fun getPos'(list as (l::ls),p,x,y) = 
if l = (p,(x,y)) then (x,y)
else if x=7 then
	getPos'(ls,p,0,y+1)
else 
    getPos'(ls,p,x+1,y)
	
(* getPos(board, p)
	TYPE: (piece * (int * int)) list list * piece -> int * int
	PRE: there is only one of the piece p on the board
	POST: the position in board of p
	EXAMPLE: getPos(getBoard(),Piece(White,King)) = (4,0)
*)
fun getPos (board as ([r1,r2,r3,r4,r5,r6,r7,r8]), p as Piece(suit,unit)) =
let
    val board' = r1@r2@r3@r4@r5@r6@r7@r8
in
    getPos'(board',p,0,0)
end
    
(* KingIsSafe (board, suit)
   TYPE: (piece*(int * int ))list list * suit -> bool
   PRE: The King of requested suit is in board
   POST: true if the king is not in check false otherwise
   Example: KingIsSafe(getBoard(),White) = true
   KingIsSafe([....[(Piece(White, King), (0,3)),(Piece(Black,Queen), (1,3))...].....]) = false
*)

fun KingIsSafe (board, suit) = 
    let
	val KingPos = getPos(board,Piece(suit,King))
	val Kx = #1(getPos(board,Piece(suit,King)))
	val Ky = #2(getPos(board,Piece(suit,King)))
	val EnemySuit = 
	    if suit = White then 
		Black 
	    else
		White
	val unitsBelow = if Ky < 7 then
			     getVerticalList(board,Ky,Kx,7-Ky,[])
			 else
			     []
	val unitsAbove = if suit = White then 
			     if Ky > 0 then
				 rev((Piece(getSuit(board,(Kx,0)),getPiece(board,(Kx,0))),(Kx,0))::
				     getVerticalList(board,0,Kx,7-Ky,[]))
			     else
				 []
			 else
			     if Ky > 0 then
				rev((Piece(getSuit(board,(Kx,0)),getPiece(board,(Kx,0))),(Kx,0))::
				    getVerticalList(board,0,Kx,Ky-1,[]))
			     else 
				 []

	val unitsLeft = rev(List.take(List.nth(board,Ky),Kx))
	val unitsRight = List.drop(List.nth(board,Ky),Kx+1)
	val uTL = if Ky > 0 then
		      (if (Kx > Ky) then 
			   (Piece(getSuit(board,(Kx-Ky,0)),getPiece(board,(Kx-Ky,0))),(Kx-Ky,0))::
			   getDiagonalList(board,(Kx-Ky,0),KingPos,Ky,[])
		       else if (Kx < Ky) then
			   rev((Piece(getSuit(board,(0,Ky-Kx)),getPiece(board,(0,Ky-Kx))),(0,Ky-Kx))::
			       getDiagonalList(board,(0,Ky-Kx),KingPos,Kx,[]))
		       else if (Kx = Ky) then
			   rev((Piece(getSuit(board,(0,0)),getPiece(board,(0,0))),(0,0))::
			       getDiagonalList(board,(0,0),KingPos, Kx ,[]))
		       else 
			   [])
		  else 
		      []
	val uBR = if Ky < 7 then
		      (if Kx > Ky then
			   rev((Piece(getSuit(board,(7,Ky+(7-Kx))),getPiece(board,(7,Ky+(7-Kx)))),(7,Ky+(7-Kx)))::
			       rev(getDiagonalList(board,KingPos,(7,Ky+(7-Kx)),7-Kx,[])))
		       else if Kx < Ky then
			   rev((Piece(getSuit(board,(Kx+(7-Ky),7)),getPiece(board,(Kx+(7-Ky),7))),(Kx+(7-Kx),7))::
			       rev(getDiagonalList(board,KingPos,(Kx+(7-Ky),7),7-Ky,[])))
		       else if Kx = Ky then
			   rev((Piece(getSuit(board,(7,7)),getPiece(board,(7,7))),(7,7))::
			       rev(getDiagonalList(board,KingPos,(7,7),7-Ky,[])))
		       else
			   [])
		  else []
	val uTR = if Ky > 0 andalso Ky < 7 then
		      if Kx + Ky <= 7 then
			  rev((Piece(getSuit(board,(Ky+Kx,0)),getPiece(board,(Ky+Kx,0))),(Ky+Kx,0))::
			      getDiagonalList2(board,((Kx+Ky),0),KingPos,Ky,0,[]))
		      else
			  rev((Piece(getSuit(board,(7,(Ky+Kx)-7)),getPiece(board,(7,(Ky+Kx)-7))),(7,(Ky+Kx)-7))::
			      getDiagonalList2(board,(7,(Kx+Ky)-7),KingPos,7-Kx,0,[]))
		  else 
		      []
	val uBL = 
	    if Ky > 0 andalso Ky < 7 then
		if Kx + Ky <= 7 then
		    rev((Piece(getSuit(board,(0,Ky+Kx)),getPiece(board,(0,Ky+Kx))),(0,Ky+Kx))::
			rev(getDiagonalList2(board,KingPos,(0,(Kx+Ky)),Kx,0,[])))
		else
		    rev((Piece(getSuit(board,((Ky+Kx)-7,7)),getPiece(board,((Ky+Kx)-7,7))),((Ky+Kx)-7,7))::
			rev(getDiagonalList2(board,KingPos,((Kx+Ky)-7,7),7-Ky,0,[])))
	    else []
	val Knight1 = if Kx > 0 andalso Ky < 6 then
			  [(Piece(getSuit(board,(Kx-1,Ky+2)),getPiece(board,(Kx-1,Ky+2))),(Kx-1,Ky+2))]
		      else 
			  []
	val Knight2 = if Kx > 1 andalso Ky < 7 then
			  [(Piece(getSuit(board,(Kx-2,Ky+1)),getPiece(board,(Kx-2,Ky+1))),(Kx-2,Ky+1))]
		      else 
			  []
	val Knight3 = if Kx > 1 andalso Ky > 0 then
			  [(Piece(getSuit(board,(Kx-2,Ky-1)),getPiece(board,(Kx-2,Ky-1))),(Kx-2,Ky-1))]
		      else 
			  []		
	val Knight4 = if Kx > 0 andalso Ky > 1 then
			  [(Piece(getSuit(board,(Kx-1,Ky-2)),getPiece(board,(Kx-1,Ky-2))),(Kx-1,Ky-2))]
		      else 
			  []			
	val Knight5 = if Kx < 7 andalso Ky > 1 then
			  [(Piece(getSuit(board,(Kx+1,Ky-2)),getPiece(board,(Kx+1,Ky-2))),(Kx+1,Ky-2))]
		      else 
			  []			
	val Knight6 = if Kx < 6 andalso Ky > 0 then
			  [(Piece(getSuit(board,(Kx+2,Ky-1)),getPiece(board,(Kx+2,Ky-1))),(Kx+1,Ky+2))]
		      else 
			  []
	val Knight7 = if Kx < 7 andalso Ky < 6 then			
			  [(Piece(getSuit(board,(Kx+1,Ky+2)),getPiece(board,(Kx+1,Ky+2))),(Kx+1,Ky+2))]
		      else
			  []
	val Knight8 = if Kx < 6 andalso Ky < 7 then
			  [(Piece(getSuit(board,(Kx+2,Ky+1)),getPiece(board,(Kx+2,Ky+1))),(Kx+2,Ky+1))]
		      else 
			  []
	val KnightList = Knight1@Knight2@Knight3@Knight4@Knight5@Knight6@Knight7@Knight8

	val threatsBelow = (List.filter(fn x => getPiece'(x) <> none) unitsBelow)
	val threatsAbove =  (List.filter(fn x => getPiece'(x) <>none) unitsAbove)
	val threatsLeft =  (List.filter(fn x => getPiece'(x) <>none ) unitsLeft)
	val threatsRight =  (List.filter(fn x => getPiece'(x) <>none  ) unitsRight)
	val threatsuTL =  (List.filter(fn x => getPiece'(x) <>none  ) uTL)
	val threatsuBR =  (List.filter(fn x => getPiece'(x) <>none  ) uBR)
	val threatsuTR =  (List.filter(fn x => getPiece'(x) <>none ) uTR)
	val threatsuBL =  (List.filter(fn x => getPiece'(x) <>none ) uBL)
	val threatsKnightList = List.filter(fn x => getSuit'(x) = EnemySuit) (List.filter(fn x => getPiece'(x) = Knight) KnightList)
        val pawn1 = if suit = Black then
			if Kx > 0 then
			    [(Piece(getSuit(board,(Kx-1,Ky-1)),getPiece(board,(Kx-1,Ky-1))),(Kx-1,Ky-1))]
			else 
			    []
		    else 
			if Kx > 0 then
			    [(Piece(getSuit(board,(Kx-1,Ky+1)),getPiece(board,(Kx-1,Ky+1))),(Kx-1,Ky+1))]
			else 
			    []
		val pawn2 = if suit = Black then
			if Kx < 7 then
			    [(Piece(getSuit(board,(Kx+1,Ky-1)),getPiece(board,(Kx+1,Ky-1))),(Kx+1,Ky-1))]
			else 
			    []
		    else 
			if Kx < 7 then
			    [(Piece(getSuit(board,(Kx+1,Ky+1)),getPiece(board,(Kx+1,Ky+1))),(Kx+1,Ky+1))]
			else 
			    []	
	val pawns = List.filter(fn x => getSuit'(x) = EnemySuit)(List.filter(fn x => getPiece' (x) = Pawn orelse getPiece'(x) = King) (pawn1@pawn2))
    in
	  
	if threatsKnightList = [] then
	    if threatsBelow = [] orelse getSuit'(hd(threatsBelow)) <> EnemySuit then
		if threatsAbove = [] orelse getSuit'(hd(threatsAbove)) <> EnemySuit then
		    if threatsLeft = [] orelse getSuit'(hd(threatsLeft)) <> EnemySuit then 
			if threatsRight = [] orelse getSuit'(hd(threatsRight)) <> EnemySuit then 
			    if threatsuTL = [] orelse getSuit'(hd(threatsuTL)) <> EnemySuit then 
				if threatsuBR = [] orelse getSuit'(hd(threatsuBR)) <> EnemySuit then 
				    if threatsuTR = [] orelse getSuit'(hd(threatsuTR)) <> EnemySuit then 
					if threatsuBL = [] orelse getSuit'(hd(threatsuBL)) <> EnemySuit then 
					    pawns = []
					else 
					    getPiece'(hd(threatsuBL)) <> Bishop andalso getPiece'(hd(threatsuBL)) <> Queen
				    else 
					getPiece'(hd(threatsuTR)) <> Bishop andalso getPiece'(hd(threatsuTR)) <>  Queen
				else 
				    getPiece'(hd(threatsuBR)) <> Bishop andalso getPiece'(hd(threatsuBR)) <>  Queen
			    else 
				getPiece'(hd(threatsuTL)) <> Bishop andalso getPiece'(hd(threatsuTL)) <>  Queen
			else 
			    getPiece'(hd(threatsRight)) <> Rook andalso getPiece'(hd(threatsRight)) <>  Queen
		    else 
			getPiece'(hd(threatsLeft)) <> Rook andalso getPiece'(hd(threatsLeft)) <>  Queen
		else 
		    getPiece'(hd(threatsAbove)) <> Rook andalso getPiece'(hd(threatsAbove)) <>  Queen
	    else 
		getPiece'(hd(threatsBelow)) <> Rook andalso getPiece'(hd(threatsBelow)) <>  Queen
	else 
	    false


    end

(* fun movePiece (board, (Px, Py), (Tx, Ty))
   TYPE: (piece * (int * int)) list list * (int * int) * (int * int) -> (piece * (int * int)) list list
   PRE: the requested piece to move is on the board 
        Px Py Tx and Ty are within the board coordiantes
   POST: board with the element at position (Tx, Ty) replaced with the element at position (Px, Py) leaving (Px, Py) as Empty.
   EXAMPLE: movePiece([[(Piece (White, Rook), (0, 0)), (Piece (White, Knight), (1, 0)),
     (Piece (White, Bishop), (2, 0)), (Piece (White, Queen), (3, 0)),
     (Piece (White, King), (4, 0)), (Piece (White, Bishop), (5, 0)),
     (Piece (White, Knight), (6, 0)), (Piece (White, Rook), (7, 0))],
    [(Piece (White, Pawn), (0, 1)), (Piece (White, Pawn), (1, 1)),
     (Piece (White, Pawn), (2, 1)), (Piece (White, Pawn), (3, 1)),
     (Piece (White, Pawn), (4, 1)), (Piece (White, Pawn), (5, 1)),
     (Piece (White, Pawn), (6, 1)), (Piece (White, Pawn), (7, 1))],
    [(Empty, (0, 2)), (Empty, (1, 2)), (Empty, (2, 2)), (Empty, (3, 2)),
     (Empty, (4, 2)), (Empty, (5, 2)), (Empty, (6, 2)), (Empty, (7, 2))],
    [(Empty, (0, 3)), (Empty, (1, 3)), (Empty, (2, 3)), (Empty, (3, 3)),
     (Empty, (4, 3)), (Empty, (5, 3)), (Empty, (6, 3)), (Empty, (7, 3))],
    [(Empty, (0, 4)), (Empty, (1, 4)), (Empty, (2, 4)), (Empty, (3, 4)),
     (Empty, (4, 4)), (Empty, (5, 4)), (Empty, (6, 4)), (Empty, (7, 4))],
    [(Empty, (0, 5)), (Empty, (1, 5)), (Empty, (2, 5)), (Empty, (3, 5)),
     (Empty, (4, 5)), (Empty, (5, 5)), (Empty, (6, 5)), (Empty, (7, 5))],
    [(Piece (Black, Pawn), (0, 6)), (Piece (Black, Pawn), (1, 6)),
     (Piece (Black, Pawn), (2, 6)), (Piece (Black, Pawn), (3, 6)),
     (Piece (Black, Pawn), (4, 6)), (Piece (Black, Pawn), (5, 6)),
     (Piece (Black, Pawn), (6, 6)), (Piece (Black, Pawn), (7, 6))],
    [(Piece (Black, Rook), (0, 7)), (Piece (Black, Knight), (1, 7)),
     (Piece (Black, Bishop), (2, 7)), (Piece (Black, Queen), (3, 7)),
     (Piece (Black, King), (4, 7)), (Piece (Black, Bishop), (5, 7)),
     (Piece (Black, Knight), (6, 7)), (Piece (Black, Rook), (7, 7))]],(1,1),(1,3));
=
   [[(Piece (White, Rook), (0, 0)), (Piece (White, Knight), (1, 0)),
     (Piece (White, Bishop), (2, 0)), (Piece (White, Queen), (3, 0)),
     (Piece (White, King), (4, 0)), (Piece (White, Bishop), (5, 0)),
     (Piece (White, Knight), (6, 0)), (Piece (White, Rook), (7, 0))],
    [(Piece (White, Pawn), (0, 1)), (Empty, (1, 1)),
     (Piece (White, Pawn), (2, 1)), (Piece (White, Pawn), (3, 1)),
     (Piece (White, Pawn), (4, 1)), (Piece (White, Pawn), (5, 1)),
     (Piece (White, Pawn), (6, 1)), (Piece (White, Pawn), (7, 1))],
    [(Empty, (0, 2)), (Empty, (1, 2)), (Empty, (2, 2)), (Empty, (3, 2)),
     (Empty, (4, 2)), (Empty, (5, 2)), (Empty, (6, 2)), (Empty, (7, 2))],
    [(Empty, (0, 3)), (Piece (White, Pawn), (1, 3)), (Empty, (2, 3)),
     (Empty, (3, 3)), (Empty, (4, 3)), (Empty, (5, 3)), (Empty, (6, 3)),
     (Empty, (7, 3))],
    [(Empty, (0, 4)), (Empty, (1, 4)), (Empty, (2, 4)), (Empty, (3, 4)),
     (Empty, (4, 4)), (Empty, (5, 4)), (Empty, (6, 4)), (Empty, (7, 4))],
    [(Empty, (0, 5)), (Empty, (1, 5)), (Empty, (2, 5)), (Empty, (3, 5)),
     (Empty, (4, 5)), (Empty, (5, 5)), (Empty, (6, 5)), (Empty, (7, 5))],
    [(Piece (Black, Pawn), (0, 6)), (Piece (Black, Pawn), (1, 6)),
     (Piece (Black, Pawn), (2, 6)), (Piece (Black, Pawn), (3, 6)),
     (Piece (Black, Pawn), (4, 6)), (Piece (Black, Pawn), (5, 6)),
     (Piece (Black, Pawn), (6, 6)), (Piece (Black, Pawn), (7, 6))],
    [(Piece (Black, Rook), (0, 7)), (Piece (Black, Knight), (1, 7)),
     (Piece (Black, Bishop), (2, 7)), (Piece (Black, Queen), (3, 7)),
     (Piece (Black, King), (4, 7)), (Piece (Black, Bishop), (5, 7)),
     (Piece (Black, Knight), (6, 7)), (Piece (Black, Rook), (7, 7))]]

     WHAT HAPEENED??:(pawn moved from (1,1) to (1,3))

*)

fun movePiece (board, PiecePos as (Px,Py), TargetPos as(Tx,Ty)) = 
    let
	val MovingSuit = getSuit(board,PiecePos)
	val MovingPiece = getPiece(board, PiecePos)
    in
	(case MovingPiece of 
	     Pawn =>
	     if validMove(board,PiecePos,TargetPos,1)andalso KingIsSafe( updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,Pawn)),MovingSuit) then 
		 updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,Pawn))
	     else 
		 board
	   |Rook =>
	    if validMove(board,PiecePos,TargetPos,2) then
		if(Px=Tx)andalso KingIsSafe(updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,Rook)),MovingSuit) then
		    updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,Rook))
		else if KingIsSafe(updateBoardHorizontal(board,PiecePos,TargetPos,Piece(MovingSuit,Rook)),MovingSuit) then
				 updateBoardHorizontal(board,PiecePos,TargetPos,Piece(MovingSuit,Rook))
		    else 
			board
	    else 
		board
	   |Knight =>
	    if validMove(board,PiecePos,TargetPos,3)andalso KingIsSafe(updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,Knight)),MovingSuit) then
		updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,Knight))
	    else 
		board
	   |Bishop =>
	    if validMove(board, PiecePos, TargetPos, 4)andalso KingIsSafe(updateBoardVertical(board, PiecePos, TargetPos, Piece(MovingSuit, Bishop)),MovingSuit) then
		updateBoardVertical(board, PiecePos, TargetPos, Piece(MovingSuit, Bishop))
	    else 
		board
	   |Queen => 
	    if Px = Tx andalso Ty <> Py  then 
		if validMove(board, PiecePos, TargetPos, 2) andalso KingIsSafe(updateBoardVertical (board,PiecePos,TargetPos,Piece(MovingSuit,Queen)),MovingSuit) then 
		    updateBoardVertical (board,PiecePos,TargetPos,Piece(MovingSuit,Queen))
		else
		    board
	    else if	validMove(board, PiecePos, TargetPos, 2) then 
		if Px <> Tx andalso Ty = Py andalso KingIsSafe(updateBoardHorizontal(board,PiecePos,TargetPos,Piece(MovingSuit,Queen)),MovingSuit) then 
		    updateBoardHorizontal(board,PiecePos,TargetPos,Piece(MovingSuit,Queen))
		else 
		    board
	    else if Px <> Tx andalso Ty <> Py then 
		if validMove(board, PiecePos, TargetPos, 4) andalso KingIsSafe(updateBoardVertical(board, PiecePos, TargetPos, Piece(MovingSuit, Queen)),MovingSuit) then
		    updateBoardVertical(board, PiecePos, TargetPos, Piece(MovingSuit, Queen))
		else
		    board
	    else 
		board
	   |King =>
	    if getSuit(board,TargetPos)<> MovingSuit then
		if Px=Tx andalso Ty=Py+1 orelse Ty=Py-1 andalso KingIsSafe(updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,King)),MovingSuit) then
		    updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,King))
		else if Px+1=Tx orelse Px-1=Tx then
		    if Py=Ty andalso KingIsSafe(updateBoardHorizontal(board,PiecePos,TargetPos,Piece(MovingSuit,King)),MovingSuit) then
			updateBoardHorizontal(board,PiecePos,TargetPos,Piece(MovingSuit,King))
		    else if Ty-1=Py orelse Ty+1=Py andalso KingIsSafe(updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,King)),MovingSuit) then
			updateBoardVertical(board,PiecePos,TargetPos,Piece(MovingSuit,King))
		    else board
		else board
	    else board
	   |_=> raise NoPiece
	)
    end

	
	
	
	
	
(*All tests should evaluate to true, otherwise something is wrong in the code*)   

(*
First part: Attempts to move a rook through the pawn at (0,6). Since the path is not clear this should not
be possible and it should thus only return the received board again.
Second part: Attempts to move the rook diagonally. Since a rook only can move horizontally and vertically
this should not be possible.
Third part: Attempts to first move the rook vertically then to horizontally
*)

fun testRook() = (*First*)if movePiece(getBoard(), (0,7), (0,4)) = getBoard() 
andalso (*Second*)
movePiece(movePiece(getBoard(), (1,6),(1,5)), (0,7), (2,5)) =  
movePiece(getBoard(), (1,6),(1,5)) 
andalso (*Third*)
getPiece(movePiece(movePiece(movePiece(getBoard(), (0,6),(0,4)), (0,7), (0,5)), (0,5), (4,5)), (4,5)) = 
Rook (*checks that the Rook has moved to the correct position*) 
then true else false

(*
First part: moves a pawn forward leaving an empty path for the bishop. First moves the bishop in a diagonal
in one direction then diagonal in the other.
Second part: Attempts to move a bishop through the pawn at (4,6). Since the path is not clear this should not
be possible and it should thus only return the received board again. 
Third part: Attempts to move the Bishop in a forward manner which should not be possible and therefore that 
move should not be made.
*)
fun testBishop() = (*First*)if getPiece(movePiece(movePiece(movePiece(getBoard(), (4,6), (4,4)), (5,7), (3,5)), (3,5),(1,3)), (1,3)) = 
Bishop 
andalso (*Second*)
movePiece(getBoard(), (5,7), (3,5)) = getBoard()
andalso (*Third*)
movePiece(movePiece(movePiece(getBoard(), (4,6), (4,4)), (5,7), (3,5)), (3,5), (3,3)) =
movePiece(movePiece(getBoard(), (4,6), (4,4)), (5,7), (3,5))
 then true else false


(*
Since the queen uses the same functions and the Bishop and Rook no special test-cases is needed for the queen
*)

(*
First part: Checks that the knight can "Jump" over other pieces.
Second part: Since the square (2,5) is occupied by a pawn the Knight should be unable to go there.
*)
fun testKnight() = (*First*)if getPiece(movePiece(getBoard(), (1,7), (2,5)), (2,5)) = 
Knight 
andalso (*Second*)
movePiece(movePiece(getBoard(), (2,6) ,(2,5)), (1,7),(2,5)) = movePiece(getBoard(), (2,6), (2,5))
then true else false

(*
Moves the black queen in front of the White king making the White king "unsafe"
*)
fun test1 () = if KingIsSafe(movePiece(movePiece(movePiece(movePiece(getBoard(), (3,6) ,(3,4)), (3,7),(3,5)),(3,5),(4,5)), (4,5),(4,1)), White) then false else true

(*
Attempts to move the knight while the king is in chess.
since this does not remove the check the board should remain unchanged when the knight is attempted to be moved.
*)
fun test2 () = if movePiece(movePiece(movePiece(movePiece(movePiece(getBoard(), (3,6) ,(3,4)), (3,7),(3,5)),(3,5),(4,5)), (4,5),(4,1)),(1,0),(2,2)) =
movePiece(movePiece(movePiece(movePiece(getBoard(), (3,6) ,(3,4)), (3,7),(3,5)),(3,5),(4,5)), (4,5),(4,1)) then true else false

(*
The queen is in front of the king and the bishop at (5,0) attempts to capture the queen,
since this will remove the "check" status on the king it will be a valid move and the king
should now be safe.
*)
fun test3() = if KingIsSafe(movePiece(movePiece(movePiece(movePiece(movePiece(getBoard(), (3,6) ,(3,4)), (3,7),(3,5)),(3,5),(4,5)), (4,5),(4,1)), (5,0) ,(4,1)), White) then true else false

(*
The queen is in front of the king with a couple of squares as space between. The bishop at (5,0) attempts to
move in the way to remove the check.
*)
fun test4() =  if KingIsSafe(movePiece(movePiece(movePiece(movePiece(movePiece(movePiece(getBoard(), (3,6) ,(3,4)), (3,7),(3,5)),(3,5),(4,5)), (4,1),(4,3)),(4,5), (4,3)), (5,0), (4,1)), White) then true else false
(*
The king will attempts to move himself into a check position which should not be allowed.
*)





fun runtests(x) = 
let 
val x = x 
in
	case x of 

		0 => testRook() 
			| 1 => testBishop() 
			| 2 => testKnight() 
			| 3 => test1() 
			| 4 => test2()  
			| 5 => test3()  
			| 6 => test4() 
			
end;

runtests 0;
runtests 1;
runtests 2;
runtests 3;
runtests 4;
runtests 5;
runtests 6;






