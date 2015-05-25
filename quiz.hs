import Control.Concurrent
import Control.Monad
import Data.List
import Data.Maybe
import System.Console.ANSI
import System.Directory
import System.IO
import System.Posix.Files
import System.Random
import System.Random.Shuffle
import qualified Data.Heap as H

-- Math questions

data Op = Plus | Minus | Mult | Div deriving (Eq, Ord, Show, Read)

data MathQuestion = MathQuestion Op Int Int deriving (Eq, Ord, Show, Read)

eval (MathQuestion x a b) = op x a b where
    op Plus  = (+)
    op Minus = (-)
    op Mult  = (*)
    op Div   = div

equation (MathQuestion op a b) = show a ++ opString op ++ show b

opString Plus  = " + "
opString Minus = " - "
opString Mult  = " × "
opString Div   = " ÷ "

instance Question MathQuestion where
    question q = equation q ++ " = "
    answer     = show . eval
    check q r  = read r == eval q

timesTable n max = [ MathQuestion Mult n x | x <- [0..max] ] ++ [ MathQuestion Mult x n | x <- [0..max] ]

-- Text questions

data TextQuestion = TextQuestion String String deriving (Eq, Ord, Show, Read)

instance Question TextQuestion where
    question (TextQuestion q a) = q ++ "? "
    answer (TextQuestion q a)   = a
    check (TextQuestion _ a) r  = r == a

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

-- Generic quiz foo

class Question q where
    question, answer :: q -> String
    check            :: q -> String -> Bool

data ToAsk q = ToAsk q Int Int deriving (Eq, Ord, Show, Read)

data Quiz q = Quiz { count :: Int
                   , asked :: H.MinPrioHeap Int (ToAsk q)
                   , unasked :: [q]
                   , limit :: Int
                   , retired :: Int
                   } deriving (Show, Read)

-- Extract the next question from the quiz.
toAsk :: Quiz q -> Maybe (ToAsk q, Quiz q)

toAsk (Quiz _ asked [] _ _) | H.isEmpty asked = Nothing

toAsk (Quiz count asked [] limit retired) = do
  ((_, a), rest) <- H.view asked
  return (a, Quiz count rest [] limit retired)

toAsk (Quiz count asked (q:qs) limit retired) | H.isEmpty asked = Just (newToAsk q, Quiz count asked qs limit retired)

toAsk (Quiz count asked (q:qs) limit retired) = Just (if c <= count then oldQuestion else newQuestion) where
    ((c, a), rest) = fromJust $ H.view asked
    newQuestion = (newToAsk q, Quiz count asked qs limit retired)
    oldQuestion = (a, Quiz count rest (q:qs) limit retired)

newToAsk q = ToAsk q 1 0

-- Update quiz based on the question just asked and whether it was answered correctly.
answered :: ToAsk q -> Bool -> Quiz q -> Quiz q
answered (ToAsk q gap correct) ok (Quiz count asked qs limit retired) = Quiz (count + 1) asked' qs limit retired' where
    ask      = ToAsk q gap' correct'
    asked'   = if correct' == limit then asked else H.insert (count + gap', ask) asked
    gap'     = if ok then gap * 2 else 1
    correct' = if ok then correct + 1 else 0
    retired' = if correct' == limit then retired + 1 else retired

stats (Quiz count asked unasked limit retired) = "done: " ++ show retired ++ "; in play: " ++ show (H.size asked) ++ "; unasked: " ++ show (length unasked) ++ "; limit: " ++ show limit

shuffledQuestions qs = shuffle' qs (length qs)

quiz :: Int -> [q] -> Quiz q
quiz lim qs = Quiz 0 H.empty qs lim 0

-- Console IO quizzer

prompt :: Read a => String -> IO a
prompt p = liftM read (prompt' p)

prompt' :: String -> IO String
prompt' p = do
  putStr $ p ++ " "
  hFlush stdout
  getLine

runQuiz :: (Question q, Show q) => Quiz q -> IO ()
runQuiz quiz = run (toAsk quiz) where
    run (Just (ToAsk q gap correct, qz)) = do
      clearScreen
      putStrLn $ stats qz
      ok <- askIt q
      let qz' = answered (ToAsk q gap correct) ok qz
      saveQuiz qz' "quiz.dat"
      runQuiz qz'

    run Nothing = do
      removeFile "quiz.dat"
      putStrLn "Looks like you've got them all."

askIt :: Question q => q -> IO Bool
askIt q = do
  ok <- liftM (check q) (prompt' (question q))
  putStrLn $ if ok then "Right!" else "Oops. The answer is " ++ answer q ++ "."
  threadDelay 300000
  if not ok then getLine else return ""
  return ok

saveQuiz :: Show q => Quiz q -> FilePath -> IO ()
saveQuiz q p = writeFile p (show q)

getQuiz :: Bool -> IO (Quiz MathQuestion)
getQuiz exists = if exists then loadQuiz "quiz.dat" else newQuiz

loadQuiz :: Read q => FilePath -> IO (Quiz q)
loadQuiz p = readFile p >>= readIO

newQuiz :: IO (Quiz MathQuestion)
newQuiz = do
  n <- prompt "Which times table?"
  l <- prompt "Correct answers required?"
  liftM (quiz l . shuffledQuestions (timesTable n 20)) getStdGen

main = fileExist "quiz.dat" >>= getQuiz >>= runQuiz
