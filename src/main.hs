import Data.List
import Control.Monad
import Data.Maybe

data Symbol = A | B deriving (Show, Eq)
type Wword = [Symbol]

m1 :: Wword
m1 = [A,B,A,A,B]

type State = String -- deriving (Show, Eq)
type TransitionTable = [(State,Symbol,State)]
data DFA = DFA 
  {   start :: State,
      accept :: [State],
      trans :: TransitionTable
  } deriving (Show, Eq)

auto1 :: DFA
auto1 = DFA 
        {
                start = "1",
                accept = ["2"],
                trans = [("1",A,"2"),("2",A,"2"),("2",B,"2")]
        }



-- FUNCTIONS --
-- Finite State Machine acceptance test
nextState :: TransitionTable -> State -> Symbol -> Maybe State
nextState t s0 l = case 
       (find 
                (\(s1,lt,s2) -> (s1==s0) && (lt==l)) 
                t
        ) of
               Nothing -> Nothing
               Just (_,_,s1) -> Just s1

nextStateW :: TransitionTable -> State -> Wword -> Maybe State
nextStateW t s0 w =
        foldl
                (\ms l -> ms >>= (\s -> nextState t s l))
                (Just s0)
                w

finalStateW :: DFA -> Wword -> Maybe State
finalStateW d w =
        nextStateW (trans d) (start d) w

isAccepted :: DFA -> Wword -> Bool
isAccepted d w = elem (finalStateW d w) (map (\s -> Just s) (accept d))


-- FSM arithmetic
stateProd :: State -> State -> State
stateProd s1 s2 = "("++s1++","++s2++")"

interDFA :: DFA -> DFA -> DFA
interDFA a1 a2 = DFA {
                start = stateProd (start a1) (start a2),
                accept = [ stateProd x y |
                                x <- (accept a1), y <- (accept a2)],
                trans = [ ] --TODO
        }
-- Regex to FSM
