import Data.List
import System.IO
import Data.Maybe
import qualified Data.Heap as H

-- Math questions

data Op = Plus | Minus | Mult | Div deriving (Eq, Ord)

instance Show Op where
    show Plus  = " + "
    show Minus = " - "
    show Mult  = " * "
    show Div   = " / "

data MathQuestion = MathQuestion Op Int Int deriving (Eq, Ord)

eval :: MathQuestion -> Int
eval (MathQuestion Plus a b)  = a + b
eval (MathQuestion Minus a b) = a - b
eval (MathQuestion Mult a b)  = a * b
eval (MathQuestion Div a b)   = a `div` b

instance Show MathQuestion where
    show (MathQuestion op a b) = (show a) ++ (show op) ++ (show b)

instance Question MathQuestion where
    question q = (show q) ++ " = "
    answer     = show . eval
    check q s  = read s == eval q

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

data ToAsk q = ToAsk q Int deriving (Eq, Ord, Show)

data Quiz q = Quiz { count :: Int
                   , asked :: H.MinPrioHeap Int (ToAsk q)
                   , unasked :: [q]
                   } deriving (Show)

newToAsk q = ToAsk q 1

-- Pull a question from either the heap of already asked questions (if
-- one is due) or the list of unasked questions.
nextQuestion :: Quiz q -> Maybe (ToAsk q, Quiz q)

nextQuestion (Quiz count asked []) = do
  ((_, a), rest) <- H.view asked
  return (a, (Quiz count rest []))

nextQuestion (Quiz count asked (q:qs)) | H.isEmpty asked = Just (newToAsk q, Quiz count asked qs)

nextQuestion (Quiz count asked (q:qs)) = Just (if c <= count then oldQuestion else newQuestion) where
    ((c, a), rest) = fromJust $ H.view asked
    newQuestion = (newToAsk q, Quiz count asked qs)
    oldQuestion = (a, Quiz count rest (q:qs))

-- Take the question just asked and whether it was answered correctly and put it back into the Quiz.
answered :: (ToAsk q) -> Bool -> (Quiz q) -> (Quiz q)
answered (ToAsk q gap) ok (Quiz count asked qs) = Quiz (count + 1) (H.insert nextTime asked) qs where
    nextTime = (count + newGap, ToAsk q newGap)
    newGap = if ok then gap * 2 else 1

quiz qs = Quiz 0 H.empty qs

getAnswer :: IO Int
getAnswer = readLn

askIt :: (Question q) => q -> IO Bool
askIt q = do
  putStr $ question q
  hFlush stdout
  x <- getLine
  let ok = check q x
  putStrLn $ if ok then "Right!" else "Oops. The answer is " ++ (answer q) ++ "."
  return ok

foo (Just (ToAsk q gap, qz)) = do
  ok <- askIt q
  runQuiz (answered (ToAsk q gap) ok qz)

foo Nothing = print "Done."

runQuiz quiz = foo (nextQuestion quiz)

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

main = runQuiz $ quiz $ take 2 spanishQuestions
