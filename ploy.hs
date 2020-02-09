-- CALL ME BABY
list_moves :: String -> String
list_moves xs = to_string (get_moves2 xs)

-- format: [a9-b8-0,c7-c7-2]
to_string :: [String] -> String
to_string strs = 
	"[" ++ init (init (concatMap (++ ", ") strs)) ++ "]"

-- param: "board, w/b"
get_moves2 :: String -> [String]
get_moves2 str = get_moves (init str) (last str)

data Token = Token {start :: (Int, Int), dirs :: [Int]}
	deriving Show

-- param: encoded board string, player turn
-- return: string list of all possible moves
get_moves :: String -> Char -> [String]
get_moves board color = zuge tokens f_fields v_fields
	where (tokens, f_fields, v_fields) = get_info board color

-- param: encoded board string, player turn
-- return: player tokens, free fields, valid fields
get_info :: String -> Char -> ([Token], [(Int, Int)], [(Int, Int)])
get_info board color = 
	(generate_tokens bstr color, get_free_fields bstr, get_valid_fields bstr color)
	where bstr = parse_board board

-- sames as below but remove all fields with pieces on them
get_free_fields :: [String] -> [(Int, Int)]
get_free_fields bstrings =
	map get_pos (map fst (filter (\x -> snd x == "0") (enum bstrings)))

-- given list of board strings and player turn color
-- return valid fields 
get_valid_fields :: [String] -> Char -> [(Int, Int)]
get_valid_fields bstrings c =
	map get_pos (map fst (filter (\x -> head (snd x) /= c) (enum bstrings)))

-- given list of board element strings & player color (w/b)
-- returns token list of corresponding player
generate_tokens :: [String] -> Char -> [Token]
generate_tokens board_list a =
	[Token (get_pos a) (get_dirs b) | (a, b) <- filter_color (enum board_list) a]

-- given (correctly formatted w/ 81 entries) board string
-- returns list of strings: zeros if empty, compacted strings otherwise
parse_board :: String -> [String]
parse_board xs 
	| length (clean_line xs) `mod` 9 == 8 = clean_line xs ++ ["0"]
	| length (clean_line xs) `mod` 9 == 0 = clean_line xs
	| otherwise = error "incorrect format"

-- returns list of strings of given color
filter_color :: [(Int, String)] -> Char -> [(Int, String)]
filter_color list a = filter (\x -> head (snd x) == a) list

-- (x: col, y: row)
-- enumerate [1..81]: (1,9), (2,9) ... (9,9), 
--					  (1,8), (2,8) ... (9,8), 
--				  ... (1,1), (2,1) ... (9,1)
get_dirs :: String -> [Int]
get_dirs (x:xs) = get_directions (read xs :: Int)

get_pos :: Int -> (Int, Int)
get_pos n = (get_x n, get_y n)

get_x n = if n `mod` 9 /= 0 then n `mod` 9
			else 9

get_y n = if n `mod` 9 /= 0 then y
			else y + 1
	where y = 9 - (n `div` 9)

enum = zip [1..]

-- returns list of 'cleaned' entries
clean_line :: String -> [String]
clean_line line = map clean_string (thru_line line)

clean_string :: String -> String
clean_string xs 
	| head xs == ' ' && last xs == ' ' = init (tail xs)
	| head xs == ' ' = tail xs
	| last xs == ' ' = init xs
	| otherwise = xs

thru_line :: String -> [String]
thru_line [] = []
thru_line line = f x ++ thru_line (next_entry line)
	where x = takeWhile (\x -> x /= ',' &&  x /= '/') line

next_entry :: String -> String
next_entry line = drop 1 (dropWhile (\x -> x /= ',' &&  x /= '/') line)

f :: String -> [String]
f x = if x == "" || x == " " then ["0"] else [x]

-- given int returns directions list
get_directions :: Int -> [Int]
get_directions n = [a | (a, b) <- zip [7,6,5,4,3,2,1,0] xs, b == 1]
	where xs = to_binary n

-- binary list
to_binary :: Int -> [Int]
to_binary n = fill (to_bin2 n)

-- fill ggf. list with 8 entries
fill :: [Int] -> [Int]
fill xs = 
	if length xs < 8 then fill ([0] ++ xs)
	else xs

to_bin2 :: Int -> [Int]
to_bin2 n = tail (to_bin n)

-- from 'ehird' in stackoverflow: 'How to implement decimal
-- 		to binary conversion'
to_bin :: Int -> [Int]
to_bin 0 = [0]
to_bin n 
	| n `mod` 2 == 1 = to_bin (n `div` 2) ++ [1]
	| n `mod` 2 == 0 = to_bin (n `div` 2) ++ [0]

-- generate all moves for list of tokens
zuge :: [Token] -> [(Int, Int)] -> [(Int, Int)] -> [String]
zuge tokens f_fields v_fields =
	concat [zuge_any t f_fields v_fields | t <- tokens] 

-- generate all moves for any token
zuge_any :: Token -> [(Int, Int)] -> [(Int, Int)] -> [String]
zuge_any t f_fields v_fields =
	if length (dirs t) > 1 then zuge_generic t f_fields v_fields
	else zuge_generic t f_fields v_fields ++ zuge_shield t f_fields v_fields

-- generate extra moves for shields 
-- i.e., rotation after ea/ move
zuge_shield :: Token -> [(Int, Int)] -> [(Int, Int)] -> [String]
zuge_shield shield f_fields v_fields =
	concat [add_rotations move | move <- token_move shield f_fields v_fields]

-- for given non-shield token, generate all move strings
zuge_generic :: Token -> [(Int, Int)] -> [(Int, Int)] -> [String]
zuge_generic token f_fields v_fields = 
	bewegung_zuge token f_fields v_fields ++ rotation_zuge token
	
-- get moves with no rotation
bewegung_zuge :: Token -> [(Int, Int)] -> [(Int, Int)] -> [String]
bewegung_zuge token f_fields v_fields =
	[m ++ "-0" | m <- token_move token f_fields v_fields]

-- generate rotation moves for non-displacement
rotation_zuge :: Token -> [String]
rotation_zuge token = add_rotations (a ++ "-" ++ a)
	where a = pos_string (start token)

-- add rotation string to given move string
add_rotations :: String -> [String]
add_rotations move = [move ++ "-" ++ [a] | a <- ['1'..'7']]

-- ESSENCE
-- returns list of start-end moves
token_move :: Token -> [(Int, Int)] -> [(Int, Int)] -> [String]
token_move token f_fields v_fields =
	[prefix ++ z | z <- token_ziel token f_fields v_fields]
	where prefix = pos_string (start token) ++ "-"

-- given token & valid fields returns list of target strings
token_ziel :: Token -> [(Int, Int)] -> [(Int, Int)] -> [String]
token_ziel token f_fields v_fields = 
	list_out (targets (start token) (dirs token) f_fields v_fields)

-- convert list of tupel positions to strings
list_out :: [(Int, Int)] -> [String]
list_out ausgabe = [pos_string t| t <- ausgabe]

--convert position tuple to position string
pos_string :: (Int, Int) -> String
pos_string (x, y) = [a] ++ show y
	where a = fst (head [(h,g)|(h,g) <- zip ['a'..'i'] [1..9], g == x])

-- given (start, directions, free_fields, valid_fields)
-- returns valid positions
targets :: (Int, Int) -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
targets start dirs f_fields v_fields = 
	concat [valid_targets_to_n start d f_fields v_fields n | d <- dirs]
	where n = if length dirs < 4 then length dirs else 1 

-- given (start, direction, free_fields, valid_fields, steps) 
-- returns list of valid positions up to n steps
valid_targets_to_n :: (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)] -> Int -> [(Int, Int)]
valid_targets_to_n start dir f_fields v_fields n = 
		[tgt | tgt <- get_targets_to_n start dir f_fields v_fields n, tgt /= (-1, -1)]

-- list of all positions w/in n steps
get_targets_to_n :: (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)] -> Int -> [(Int, Int)]
get_targets_to_n start dir f_fields v_fields n =
	if n == 1 then get_target_n start dir f_fields v_fields n : []
		else get_target_n start dir f_fields v_fields n : get_targets_to_n start dir f_fields v_fields (n-1)

-- get valid targets with n steps
-- returns position if valid, else (-1, -1)
get_target_n :: (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)] -> Int -> (Int, Int)
get_target_n start dir free_fields valid_fields n = 
	if try_n_move start dir free_fields valid_fields n
		then get_end_n start dir n
	else (-1, -1)

-- get target w/ n steps
-- (start, direction, steps)
get_end_n :: (Int, Int) -> Int -> Int -> (Int, Int)
get_end_n start dir n 
	| n == 1 = get_end start dir
	| n == 2 = get_end (get_end start dir) dir
	| n == 3 = get_end (get_end (get_end start dir) dir) dir
	| otherwise = error "n not in range"

-- (start, direction, free fields, valid fields, steps) -> Bool
try_n_move :: (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)] -> Int -> Bool 
try_n_move start dir free_fields valid_fields n
	| n == 1 = try_single_move start dir valid_fields
	| n == 2 = try_2_move start dir free_fields valid_fields
	| n == 3 = try_3_move start dir free_fields valid_fields
	| otherwise = error "n not in range[1,3]"

try_2_move :: (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)] -> Bool
try_2_move start dir free_fields valid_fields =
	if try_single_move start dir free_fields == True then 
		try_single_move (get_end start dir) dir valid_fields
	else
		False

try_3_move :: (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)] -> Bool
try_3_move start dir free_fields valid_fields =
	if try_single_move start dir free_fields == True then
		try_2_move (get_end start dir) dir free_fields valid_fields
	else
		False

-- given start, direction, fields
-- return true if move valid
try_single_move :: (Int, Int) -> Int -> [(Int, Int)] -> Bool
try_single_move start dir fields = try_end (get_end start dir) fields

-- param: position, fields list
--returns true if position available
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

--------------------------------------------------------

-- NOT NEEDED:
-- convert list of string positions to tupels
list_in :: ([String]) -> [(Int, Int)]
list_in eingabe = [pos_tupel s | s <- eingabe]

--string to position tupel
pos_tupel :: [Char] -> (Int, Int)
pos_tupel (a:b:_) = (x, read[b] :: Int)
	where x = snd (head [(h,g)|(h,g) <- zip ['a'..'i'] [1..9], h == a])

print_tokens :: [Token] -> [((Int, Int), [Int])]
print_tokens tokens = 
	[(start t, dirs t) | t <- tokens]

filter_out_rotations :: [String] -> [String]
filter_out_rotations moves =
	filter (\x -> last x == '0') moves


