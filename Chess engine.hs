import Data.Char

type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location
			| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

setBoard :: Board
setBoard = (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),
	Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
	P ('h',2),P ('g',2),P ('f',2),P ('e',2),
	P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,
	[R ('h',8),N ('g',8),B ('f',8),K ('e',8),
	Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
	P ('h',7),P ('g',7),P ('f',7),P ('e',7),
	P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
	

minus:: Char -> Char
minus a = toEnum (fromEnum a - 1)
minus2 a b = toEnum (fromEnum a - fromEnum b)
visualizeBoard:: Board -> String
visualizeBoard (color, w, b) =   "    a    b    c    d    e    f    g    h   \n"
						  ++ makeAll w b 8
						  ++"\nTurn: " ++ show color
						  
getColor White = "W"
getColor Black = "B"

getCons (P loc) = P
getCons (N loc) = N
getCons (R loc) = R
getCons (B loc) = B
getCons (Q loc) = Q
getCons (K loc) = K

opposite White = Black
opposite Black = White



getPiece _ [] []  _ = "  "
getPiece _ [] bp loc = getPiece Black bp [] loc
getPiece color ((P (x,y)):t) bp (a,b) = if x == a && y == b then "P" ++ getColor color else getPiece color t bp (a,b)
getPiece color ((N (x,y)):t) bp (a,b) = if x == a && y == b then "N" ++ getColor color else getPiece color t bp (a,b)
getPiece color ((K (x,y)):t) bp (a,b) = if x == a && y == b then "K" ++ getColor color else getPiece color t bp (a,b)
getPiece color ((Q (x,y)):t) bp (a,b) = if x == a && y == b then "Q" ++ getColor color else getPiece color t bp (a,b)
getPiece color ((R (x,y)):t) bp (a,b) = if x == a && y == b then "R" ++ getColor color else getPiece color t bp (a,b)
getPiece color ((B (x,y)):t) bp (a,b) = if x == a && y == b then "B" ++ getColor color else getPiece color t bp (a,b)



makeRow wp bp ('i',_) = " |\n"
makeRow wp bp (a,b) = " | " ++ (getPiece White wp bp (a,b)) ++ makeRow wp bp (succ a,b)


makeAll _ _ 0 = "" 
makeAll wp bp b = show b ++ makeRow wp bp ('a', b) ++ makeAll wp bp (b-1)


withinBoard (x,y) = x >= 'a' && x <= 'h' && y >= 1 && y <= 8


sameLoc (x,y) (a,b) = (x,y) == (a,b)

seperate (P (a,b)) = (a,b)
seperate (N (a,b)) = (a,b)
seperate (B (a,b)) = (a,b)
seperate (K (a,b)) = (a,b)
seperate (Q (a,b)) = (a,b)
seperate (R (a,b)) = (a,b)

occupied::[Piece] -> Location -> Bool
occupied pieces (a,b) = any (sameLoc (a,b)) (map seperate pieces)

exists player color x pieces = any (==x) pieces && player == color
exists2 x pieces = any (==x) pieces

commonLegal p (player, pw, pb) end = withinBoard end && not (any (==p) pw && occupied pw end) 
									  && not (any (==p) pb && occupied pb end)

isLegal:: Piece -> Board -> Location -> Bool
isLegal (N start) (player, pw, pb) end = commonLegal (N start) (player, pw, pb) end
										  && knightMove start end
										  
isLegal (R start) (player, pw, pb) end = commonLegal (R start) (player, pw, pb) end
										  && rookMove start end && not (pathRook (pw++pb) start end)
										  
isLegal (B start) (player, pw, pb) end = commonLegal (B start) (player, pw, pb) end
										  && bishopMove start end && not (pathBishop (pw++pb) start end)
										  
isLegal (Q start) board end = commonLegal (Q start) board end && ((isLegal (B start) board end) || (isLegal (R start) board end))
isLegal (K start) (player, pw, pb) end = commonLegal (K start) (player, pw, pb) end
										 && kingMove start end -- assumed to not matter && not (any (isLegal2 (player, pw, pb) end) (pw++pb))
isLegal (P start) (player, pw, pb) end = commonLegal (P start) (player, pw, pb) end
										 && (((pawnMove start end pw pb && not (pathRook (pw++pb) start end)) && not (occupied (pw++pb) end)) || pawnAttack start end (player, pw, pb))
-- no check

isLegal2:: Board -> Location -> Piece -> Bool
isLegal2 board end piece = isLegal piece board end

knightMove:: Location -> Location -> Bool
knightMove (x,y) (a,b) = (abs (fromEnum x - fromEnum a) == 1 && abs (y-b) == 2) || (abs (fromEnum x - fromEnum a) == 2 && abs (y-b) == 1)

rookMove (x,y) (a,b) = (x==a || b==y) && not ((x,y) == (a,b))
pathRook pieces (x,y) (a,b) | a == x = any (occupied pieces) locs1
							| otherwise = any (occupied pieces) locs2 
							where locs1 = zip [x,x,x,x,x,x,x,x,x,x,x,x,x] [((min y b)+1)..((max y b)-1)]
							      locs2 = zip [(succ (min x a))..(minus (max x a))] [y,y,y,y,y,y,y,y,y,y,y]

bishopMove (x,y) (a,b) = abs (fromEnum x - y) == abs (fromEnum a - b) || (fromEnum x +y == fromEnum a + b)
pathBishop:: [Piece] -> Location -> Location -> Bool
pathBishop pieces (x,y) (a,b) | y-b == 0 = False
							  | (div (minus2 x a) (y-b)) == 1 = any (occupied pieces) locs1
							  | otherwise = any (occupied pieces) locs2
							  where locs1 = zip [(succ (min x a))..(minus (max x a))] [((min y b)+1)..((max y b)-1)]
							        locs2 = zip [(succ (min x a))..(minus (max x a))] (reverse [((min y b)+1)..((max y b)-1)])

kingMove (x,y) (a,b) = abs (fromEnum x - fromEnum a) <= 1 && abs (y - b) <= 1 && not (sameLoc (x,y) (a,b))


pawnMove (x,2) (a,b) pw pb = x==a && (b == 3 || b == 4) && exists2 (P (x,2)) pw
pawnMove (x,7) (a,b) pw pb = x==a && (b == 6 || b == 5) && exists2 (P (x,7)) pb
pawnMove (x,y) (a,b) pw pb | exists2 (P (x,y)) pw  = x==a && (b-y == 1)
						 | otherwise = x == a && (y-b == 1) && exists2 (P (x,y)) pb

pawnAttack (x,y) (a,b) (White, pw, pb) = abs (minus2 x a) == 1 && (b-y == 1) && occupied pb (a,b)
pawnAttack (x,y) (a,b) (Black, pw, pb) = abs (minus2 x a) == 1 && (y-b == 1) && occupied pw (a,b)

suggestMove:: Piece -> Board -> [Location]
suggestMove piece board = filter (isLegal piece board) [(x,y) | x <- ['a'..'h'], y <- [1..8]]


changePos p1 dest p2 | p1 == p2 = (getCons p1) dest
					 | otherwise = p2
move:: Piece -> Location -> Board -> Board
move piece loc (player, pw, pb) | not (exists player Black piece pb || exists player White piece pw) = error ("This is "++ show player ++" player\'s turn, "++ show (opposite player) ++ " can\'t move.")
								| not (isLegal piece (player, pw, pb) loc) = error ("Illegal move for piece "++ show piece)
								| otherwise = (opposite player, map (changePos piece loc) pw, map (changePos piece loc) pb)
								
{-
 [N ('d',6),B ('e',6),B ('f',4),N ('f',3),P ('h',2),P ('g',2),Q ('e',2),P ('c',2),P ('b',2),P ('a',2),K ('c',1),R ('d',1),R ('h',1)],[Q ('a',8),B ('e',8),R ('f',8),K ('g',8),P ('e',7),B ('g',7),P ('h',7),P ('g',6),N ('f',7),P ('c',5),P ('f',5),P ('a',4),P ('b',4)])
-}