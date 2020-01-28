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
	| otherwise	= error "unknwon direction"