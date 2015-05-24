import Data.List
import System.IO
import Data.Maybe
import qualified Data.Heap as H

data Op = Plus | Minus | Mult | Div deriving (Eq, Ord)

instance Show Op where
    show Plus  = " + "
    show Minus = " - "
    show Mult  = " * "
    show Div   = " / "

data Question = Question Op Int Int deriving (Eq, Ord)

instance Show Question where
    show (Question op a b) = (show a) ++ (show op) ++ (show b)

data Asked = Asked Question Int deriving (Eq, Ord, Show)

type AlreadyAsked = H.MinPrioHeap Int Asked

data Quiz = Quiz { count :: Int
                 , asked :: AlreadyAsked
                 , unasked :: [Question]
                 } deriving (Show)

-- Pull a question from either the heap of already asked questions (if
-- one is due) or the list of unasked questions.
nextQuestion :: Quiz -> (Asked, Quiz)
nextQuestion (Quiz count asked []) = (a, (Quiz count rest [])) where
    ((_, a), rest) = fromJust (H.view asked)

nextQuestion (Quiz count asked (q:qs)) | H.isEmpty asked = (newAsked q, Quiz count asked qs)

nextQuestion (Quiz count asked (q:qs)) = if c <= count then oldQuestion else newQuestion where
    ((c, a), rest) = fromJust $ H.view asked
    newQuestion = (newAsked q, Quiz count asked qs)
    oldQuestion = (a, Quiz count rest (q:qs))

newAsked q = Asked q 1

-- Take the question just asked and whether it was answered correctly and put it back into the Quiz.
answered :: Question -> Int -> Bool -> Quiz -> Quiz
answered q gap ok (Quiz count asked qs) = Quiz (count + 1) (H.insert nextTime asked) qs where
    nextTime = (count + newGap, Asked q newGap)
    newGap = if ok then gap * 2 else 1


eval :: Question -> Int
eval (Question Plus a b)  = a + b
eval (Question Minus a b) = a - b
eval (Question Mult a b)  = a * b
eval (Question Div a b)   = a `div` b


asQuestion q    = (show q) ++ " = "
asAnswer q = (show q) ++ " = " ++ (show (eval q))

getAnswer :: IO Int
getAnswer = readLn

askIt :: Question -> IO Bool
askIt q = do
  putStr $ asQuestion q
  hFlush stdout
  x <- getAnswer
  let ok = x == eval q
  putStrLn $ if ok then "Right!" else "Oops. The answer is " ++ (show (eval q)) ++ "."
  return ok

questions = [ Question Plus a b | a <- [0..10], b <- [0..10] ]

quiz = Quiz 0 H.empty questions

runQuiz quiz = do
  --print $ "count: " ++ (show (count quiz)) ++ "; asked: " ++ (show $ sort $ H.toList (asked quiz))
  let (Asked q gap, qz) = (nextQuestion quiz)
  --print (Asked q gap)
  ok <- askIt q
  runQuiz (answered q gap ok qz)

main = runQuiz quiz
