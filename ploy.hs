data Token = Token {start :: (Int, Int), dirs :: [Int]}



--TODO:
--III: generate list of tokens, list of valid_fields
--II: parse input string



-- generate all moves for list of tokens
zuge :: [Token] -> [(Int, Int)] -> [String]
zuge tokens fields =
	concat [zuge_any t fields | t <- tokens] 

-- generate all moves for any token
zuge_any :: Token -> [(Int, Int)] -> [String]
zuge_any t fields =
	if length (dirs t) > 1 then zuge_generic t fields
	else zuge_generic t fields ++ zuge_shield t fields

-- generate extra moves for shields 
-- i.e., rotation after ea/ move
zuge_shield :: Token -> [(Int, Int)] -> [String]
zuge_shield shield fields =
	concat [add_rotations move | move <- token_move shield fields]

-- for given non-shield token, generate all move strings
zuge_generic :: Token -> [(Int, Int)] -> [String]
zuge_generic token fields = 
	bewegung_zuge token fields ++ rotation_zuge token
	
-- get moves with no rotation
bewegung_zuge :: Token -> [(Int, Int)] -> [String]
bewegung_zuge token fields =
	[m ++ "-0" | m <- token_move token fields]

-- generate rotation moves for non-displacement
rotation_zuge :: Token -> [String]
rotation_zuge token = add_rotations (a ++ "-" ++ a)
	where a = pos_string (start token)

-- add rotation string to given move string
add_rotations :: String -> [String]
add_rotations move = [move ++ "-" ++ [a] | a <- ['1'..'7']]

-- ESSENCE
-- returns list of start-end moves
token_move :: Token -> [(Int, Int)] -> [String]
token_move token fields =
	[prefix ++ z | z <- token_ziel token fields]
	where prefix = pos_string (start token) ++ "-"

-- given token & valid fields returns list of target strings
token_ziel :: Token -> [(Int, Int)] -> [String]
token_ziel token fields = 
	list_out (targets (start token) (dirs token) fields)

-- convert list of tupel positions to strings
list_out :: [(Int, Int)] -> [String]
list_out ausgabe = [pos_string t| t <- ausgabe]

--convert position tuple to position string
pos_string :: (Int, Int) -> String
pos_string (x, y) = [a] ++ show y
	where a = fst (head [(h,g)|(h,g) <- zip ['a'..'i'] [1..9], g == x])

-- given (start, directions_list, valid_fields)
-- returns valid positions
targets :: (Int, Int) -> [Int] -> [(Int, Int)] -> [(Int, Int)]
targets start dirs fields = 
	concat [valid_targets_to_n start d fields n | d <- dirs]
	where n = if length dirs < 4 then length dirs else 1 

-- given (start, direction, valid_fields, steps) 
-- returns list of valid positions up to n steps
valid_targets_to_n :: (Int, Int) -> Int -> [(Int, Int)] -> Int -> [(Int, Int)]
valid_targets_to_n start dir fields n = 
		[tgt | tgt <- get_targets_to_n start dir fields n, tgt /= (-1, -1)]

-- list of all targets w/in n steps
get_targets_to_n :: (Int, Int) -> Int -> [(Int, Int)] -> Int -> [(Int, Int)]
get_targets_to_n start dir fields n =
	if n == 1 then get_target_n start dir fields n : []
		else get_target_n start dir fields n : get_targets_to_n start dir fields (n-1)
 	
-- get targets with n steps
-- returns position if valid, else (-1, -1)
get_target_n :: (Int, Int) -> Int -> [(Int, Int)] -> Int -> (Int, Int)
get_target_n start dir fields n = 
	if try_n_move start dir fields n
		then get_end_n start dir n
	else (-1, -1)

-- get target w/ n steps
get_end_n :: (Int, Int) -> Int -> Int -> (Int, Int)
get_end_n start dir n 
	| n == 1 = get_end start dir
	| n == 2 = get_end (get_end start dir) dir
	| n == 3 = get_end (get_end (get_end start dir) dir) dir
	| otherwise = error "n not in range"

-- (start, direction, available fields, steps) -> Bool
try_n_move :: (Int, Int) -> Int -> [(Int, Int)] -> Int -> Bool 
try_n_move start dir fields n
	| n == 1 = try_single_move start dir fields
	| n == 2 = try_single_move (get_end start dir) dir fields
	| n == 3 = try_single_move (get_end (get_end start dir) dir) dir fields
	| otherwise = error "n not in range[1,3]"

-- given start, direction, free fields, try single move
try_single_move :: (Int, Int) -> Int -> [(Int, Int)] -> Bool
try_single_move start dir fields = try_end (get_end start dir) fields

--returns true if given position is playable
try_end :: (Int, Int) -> [(Int, Int)] -> Bool
try_end end fields = elem end fields

--given start field and direction (coded in int), returns end field
get_end :: (Int, Int) -> Int -> (Int, Int)
get_end (x, y) dir = (x + a, y + b)
	where (a, b) = dir_to_sum dir

--maps direction onto sum for adding to start (Int, Int)
dir_to_sum :: Int -> (Int, Int)
dir_to_sum dir 
	| dir == 0 = (0, 1)
	| dir == 1 = (1, 1)
	| dir == 2 = (1, 0)
	| dir == 3 = (1, -1)
	| dir == 4 = (0, -1)
	| dir == 5 = (-1, -1)
	| dir == 6 = (-1, 0)
	| dir == 7 = (-1, 1)
	| otherwise	= error "unknown direction"


-- NOT NEEDED:
-- convert list of string positions to tupels
list_in :: ([String]) -> [(Int, Int)]
list_in eingabe = [pos_tupel s | s <- eingabe]

--string to position tupel
pos_tupel :: [Char] -> (Int, Int)
pos_tupel (a:b:_) = (x, read[b] :: Int)
	where x = snd (head [(h,g)|(h,g) <- zip ['a'..'i'] [1..9], h == a])

