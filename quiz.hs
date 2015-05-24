import Data.List
import Data.Maybe
import System.Console.ANSI
import System.IO
import System.Random
import System.Random.Shuffle
import qualified Data.Heap as H

-- Math questions

data Op = Plus | Minus | Mult | Div deriving (Eq, Ord, Show, Read)

data MathQuestion = MathQuestion Op Int Int deriving (Eq, Ord, Show, Read)

eval (MathQuestion x a b)  = (op x) a b where
    op Plus  = (+)
    op Minus = (-)
    op Mult  = (*)
    op Div   = div

equation (MathQuestion op a b) = (show a) ++ (opString op) ++ (show b)

opString Plus  = " + "
opString Minus = " - "
opString Mult  = " × "
opString Div   = " ÷ "

instance Question MathQuestion where
    question q = (equation q) ++ " = "
    answer     = show . eval
    check q s  = read s == eval q

timesTable max = [ MathQuestion Mult x y | x <- [0..max], y <- [0..max] ]

-- Text questions

data TextQuestion = TextQuestion String String deriving (Eq, Ord)

instance Show TextQuestion where
    show (TextQuestion q a) = "Q. " ++ q ++ " A. " ++ a

instance Question TextQuestion where
    question (TextQuestion q a) = q ++ "? "
    answer (TextQuestion q a)   = a
    check (TextQuestion _ a) s  = s == a

-- Generic quiz foo

class Question q where
    question, answer :: q -> String
    check            :: q -> String -> Bool

data ToAsk q = ToAsk q Int Int deriving (Eq, Ord, Show)

data Quiz q = Quiz { count :: Int
                   , asked :: H.MinPrioHeap Int (ToAsk q)
                   , unasked :: [q]
                   , limit :: Int
                   } deriving (Show)

newToAsk q = ToAsk q 1 0

toAsk :: Quiz q -> Maybe (ToAsk q, Quiz q)
toAsk (Quiz _ asked [] _) | H.isEmpty asked= Nothing
toAsk (Quiz count asked [] limit) = H.view asked >>= (\((_, a), rest) -> return (a, (Quiz count rest [] limit)))
toAsk (Quiz count asked (q:qs) limit) | H.isEmpty asked = Just (newToAsk q, Quiz count asked qs limit)
toAsk (Quiz count asked (q:qs) limit) = Just (if c <= count then oldQuestion else newQuestion) where
    ((c, a), rest) = fromJust $ H.view asked
    newQuestion = (newToAsk q, Quiz count asked qs limit)
    oldQuestion = (a, Quiz count rest (q:qs) limit)

-- Take the question just asked and whether it was answered correctly and put it back into the Quiz.
answered :: (ToAsk q) -> Bool -> (Quiz q) -> (Quiz q)
answered (ToAsk q gap correct) ok (Quiz count asked qs limit) = Quiz (count + 1) newAsked qs limit where
    ask        = ToAsk q newGap newCorrect
    newAsked   = if newCorrect == limit then asked else H.insert (count + newGap, ask) asked
    newGap     = if ok then gap * 2 else 1
    newCorrect = if ok then correct + 1 else 0

quiz qs = Quiz 0 H.empty qs 3

getAnswer :: IO Int
getAnswer = readLn

askIt :: (Question q) => q -> IO Bool
askIt q = do
  clearScreen
  putStr $ question q
  hFlush stdout
  x <- getLine
  let ok = check q x
  putStrLn $ if ok then "Right!" else "Oops. The answer is " ++ (answer q) ++ "."
  foo <- if not ok then getLine else return ""
  return ok

foo (Just (ToAsk q gap correct, qz)) = do
  ok <- askIt q

  runQuiz (answered (ToAsk q gap correct) ok qz)

foo Nothing = putStrLn "Looks like you've got them all."

runQuiz quiz = foo (toAsk quiz)

mathQuestions = [ MathQuestion Plus a b | a <- [0..10], b <- [0..10] ]

spanishQuestions = [
 TextQuestion "el abrigo" "coat",
 TextQuestion "el lazo" "string tie",
 TextQuestion "la agujeta" "shoelace",
 TextQuestion "el overol" "overalls",
 TextQuestion "el peto" "overalls",
 TextQuestion "los calcetines" "socks",
 TextQuestion "la pajarita" "bow tie",
 TextQuestion "el calzoncillo" "under shorts",
 TextQuestion "los pantalones" "trousers",
 TextQuestion "la camisa" "shirt",
 TextQuestion "el sombrero" "hat",
 TextQuestion "la camiseta" "undershirt",
 TextQuestion "el suéter" "sweater",
 TextQuestion "la corbata" "tie",
 TextQuestion "el suspensorio" "athletic supporter",
 TextQuestion "la gorbata" "tie",
 TextQuestion "el traje" "suit",
 TextQuestion "la gorra" "ballcap",
 TextQuestion "los zapatos" "shoes"]

shuffledQuestions qs rng = shuffle' qs (length qs) rng

main = do
  rng <- getStdGen
  runQuiz $ quiz $ shuffledQuestions (timesTable 20) rng
