import Data.Text(pack, unpack, replace)
import Data.Char

-- problem definition (edit this)

possibleActions = [Mul 3, Add 8, Add 2, Rev, Mir] -- refer to FunName at line 14
maxSteps = 8
start = -1
goal = 2020


-- implementation of Calculator: The Game operations

data FunName = Mul Int | Div Int | A2B Int Int | Rev | Add Int | 
               Ins Int | Rem | Sum | Sign | Pow Int | RShift | LShift | Mir
    deriving(Show, Eq)

reverseNum :: Int -> Int
reverseNum x
    | x >= 0    = read . reverse . show $ x
    | otherwise = - (read . reverse . tail . show $ x)

aToB :: Int -> Int -> Int -> Int
aToB x a b = toNum . replace (toText a) (toText b) . toText $ x
    where toText = pack . show
          toNum  = read . unpack

-- fractional numbers break the game
strictDiv :: Int -> Int -> Maybe Int
strictDiv x y
    | x `mod` y /= 0 = Nothing
    | otherwise      = Just $ x `div` y

insert :: Int -> Int -> Int
insert x y = read $ show x ++ show y

removeDigit :: Int -> Int
removeDigit x = if null cut then 0 else read cut
    where str = show x
          cut = take (length str - 1) str

-- currently it breaks on negative numbers
-- I haven't seen a case with negative numbers and this function in the game
-- so I'm not sure how it should act
sumDigits :: Int -> Int
sumDigits = sum . map digitToInt . show

rShift :: Int -> Int
rShift x  = let digits = if x >= 0 then show x else drop 1 $ show x
                sign   = if x >= 0 then ' ' else '-'
            in  read $ sign : last digits : take (length digits - 1) digits

lShift :: Int -> Int
lShift x = let digits = if x >= 0 then show x else drop 1 $ show x
               sign   = if x >= 0 then ' ' else '-'
           in  read $ drop 1 digits ++ [head digits]

mirror :: Int -> Int
mirror x = read $ str ++ revStr
    where str    = show x
          revStr = if x >= 0 then reverse . show $ x
                             else reverse . tail . show $ x

-- numbers bigger than 6 digits break the game
capAt6Digits :: Int -> Maybe Int
capAt6Digits x
    | x > 999999 = Nothing
    | otherwise  = Just x

executeFun :: FunName -> Int -> Maybe Int
executeFun name x = case name of
    Mul y   -> capAt6Digits $ x * y
    Div y   -> x `strictDiv` y
    A2B a b -> capAt6Digits $ aToB x a b
    Rev     -> Just $ reverseNum x
    Add y   -> capAt6Digits $ x + y
    Ins y   -> capAt6Digits $ x `insert` y
    Rem     -> Just $ removeDigit x
    Sum     -> Just $ sumDigits x
    Sign    -> Just (-x)
    Pow y   -> capAt6Digits $ x ^ y
    RShift  -> Just $ rShift x
    LShift  -> Just $ lShift x
    Mir     -> capAt6Digits $ mirror x


-- functions related to actually getting the answer
-- basically generating all possible paths and choosing the successful ones

stepSingle :: ([FunName], Maybe Int) -> [([FunName], Maybe Int)]
stepSingle (steps, Nothing)  = [(steps, Nothing)]
stepSingle (steps, Just val) = [(f:steps, executeFun f val) | f <- possibleActions ]

notBroken :: ([FunName], Maybe Int) -> Bool
notBroken (_, Just _)  = True
notBroken (_, Nothing) = False

step :: [([FunName], Maybe Int)] -> [([FunName], Maybe Int)]
step = filter notBroken . concatMap stepSingle

successful :: ([FunName], Maybe Int) -> Bool
successful (_, Just val)
    | val == goal = True
    | otherwise   = False
successful (_, Nothing) = False

answer :: [[FunName]]
answer = justSteps $ map reverseSteps $ filter successful result
    where result = concat $ take (maxSteps+1) $ iterate step [([], Just start)]
          reverseSteps (steps, val) = (reverse steps, val)
          justSteps = map fst

answersInLines :: String
answersInLines = unlines . map show $ answer

main = putStr answersInLines
