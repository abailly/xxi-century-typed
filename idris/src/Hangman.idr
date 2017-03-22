import Data.Vect as V

data WordState : (remaining_guesses : Nat) -> (letters : Nat) -> Type where
  MkWS : (word : String) -> (missing : Vect letters Char) -> 
       WordState remaining_guesses letters

data Finished : Type where
  Lost : (game : WordState 0 (S letters)) -> Finished
  Won : (game : WordState (S guesses) 0)  -> Finished
 
data ValidInput : List Char -> Type where 
  Letter : (c : Char) -> ValidInput [c]
  
invalidNil : ValidInput [] -> Void
invalidNil x impossible

invalidWord : ValidInput (x :: y :: xs) -> Void
invalidWord (Letter _) impossible

isValidInput : (s : List Char) -> Dec (ValidInput s)
isValidInput [] = No invalidNil
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: y :: xs) = No invalidWord

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO ( x ** ValidInput x)
readGuess = do putStr "Guess? "
               x <- getLine 
               case isValidString x of 
                 (Yes prf) => pure (_ ** prf)
                 (No contra) => do putStr "Invalid input"
                                   readGuess

removeElem : (value : a) -> (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} ->
             Vect n a
removeElem value       (value :: ys)   {prf = Here}        = ys
removeElem {n = Z}     value (y :: []) {prf = There later} = absurd later 
removeElem {n = (S k)} value (y :: ys) {prf = There later} = y :: removeElem value ys
                                                                         
processGuess : (guess : Char) -> 
               WordState (S guesses) (S letters) -> 
               Either (WordState guesses (S letters))
                      (WordState (S guesses) letters)
processGuess guess (MkWS word missing) = 
  case isElem guess missing of 
    (Yes prf) => let newVect = (removeElem guess missing)
                 in Right (MkWS word newVect)
    (No contra) => Left (MkWS word missing)

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do
  (_ ** Letter letter) <- readGuess
  case processGuess letter st of 
    (Left l) => do putStr "Wrong !"
                   case guesses of
                     Z   => pure (Lost l)
                     S _ => game l
                     
    (Right r) => do putStr "Right !"
                    case letters of  -- initially typed 'guesses' and compiler caught the error !
                      Z   => pure (Won r)
                      S _ => game r

main : IO ()
main = do result <- game {guesses=2}
                    (MkWS "Test" ['T', 'E', 'S'])
          case result of
            Lost (MkWS word missing) =>
              putStrLn ("You lose. The word was " ++ word)
            Won game =>
              putStrLn "You win!"
