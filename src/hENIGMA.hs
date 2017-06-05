import Control.Exception
import System.IO
import Data.IORef
import Data.Char
import Cryption
import Mechanics

main :: IO ()
main = do
    clear 100
    handleStartup

-- handles the startscreen of the program
handleStartup =
    do
    hSetBuffering stdin LineBuffering
    startScreen
    clear 1
    putStrLn("  Welcome to ENIGMA.")
    clear 1
    putStrLn("  Enter q at any time to quit, r to restart.")
    clear 1
    putStrLn("  Please choose ENIGMA machine:\n   Public     (p)\n   Military   (m)")
    clear 1
    putStrLn("  Or start unit testing: \n   Unit tests (t)")
    clear 1
    line <- getLine
    if line == "p"
        then
        let
            rotorF = (fst getPublic)
        in
            do
            plugboard <- evaluate ((fst (snd getPublic)))
            reflector <- evaluate ((snd (snd getPublic)))
            clear 100
            startScreen
            clear 5
            putStrLn(" Please enter state of rotors (ex: ABC):")
            clear 1
            state <- (getLine)
            if state == "q"
                then do
                quit
                else if state == "r"
                    then do
                    main
                else do
                    rotorS <- evaluate (setStarting rotorF state)
                    handlecrypt rotorS plugboard reflector

        else if line == "m"
            then do
            clear 100
            handleMilOne
        else if line == "q"
            then do
            quit
        else if line == "r"
            then do
            main
        else if line == "t"
            then do
            runTestsAll
        else do
            clear 100
            putStrLn(" Please input a valid option")
            putStrLn("")
            handleStartup

-- runs all the test functions in the modules encryption.hs and mechanics.hs
runTestsAll = do
            clear 100
            startScreen
            putStrLn("Testing 3 functions in Encryption.hs:")
            line <- (runEnTests)
            putStrLn ((show line))
            clear 1
            putStrLn("Testing 4 functions in Mechanics.hs:")
            line <- (runMeTests)
            putStrLn ((show line))
            line <- getLine
            if line == "q"
                then do
                quit
                else if line == "r"
                    then do
                    main
                else do
                    main

-- handles the first step in the military customization
handleMilOne =
            do
            startScreen
            clear 5
            putStrLn(" Please enter the three rotors that you want to use (ex: [1,2,3])")
            clear 1
            configF <- getLine
            if configF == "q"
                then do
                quit
                else if configF == "r"
                    then do
                    main
                else do
                    configB <- (evaluate (read configF))
                    if not (configExist configB)
                        then do
                        clear 100
                        putStrLn("Index out of scope")
                        handleMilOne
                        else do
                            configS <- evaluate (read configF)
                            clear 100
                            handleMilTwo (getMilitary (configS))

configExist (x:xs)
                    |x > 0 && x < 6 && xs /= [] = configExist xs
                    |x > 0 && x < 6 = True
                    |otherwise = False

-- handles the second step in the military customization
handleMilTwo rotorS =
            do
            startScreen
            clear 5
            putStrLn(" Please select reflector a, b or c")
            clear 1
            reflectF <- getLine
            clear 1
            if reflectF == "a"
                then do
                reflector <- evaluate (makeReflector refA)
                clear 100
                handleMilThree rotorS reflector
                else if reflectF == "b"
                    then do
                    reflector <- evaluate (makeReflector refB)
                    clear 100
                    handleMilThree rotorS reflector
                else if reflectF == "c" then do
                    reflector <- evaluate (makeReflector refC)
                    clear 100
                    handleMilThree rotorS reflector
                else if reflectF == "q"
                    then do
                    quit
                else if reflectF == "r"
                    then do
                    main
                else do
                    clear 100
                    putStrLn(" Please input a valid reflector")
                    clear 1
                    handleMilTwo rotorS

-- handles the third step in the military customization
handleMilThree rotorS reflector =
            do
            startScreen
            clear 5
            putStrLn(" Please select plugboard a, b or c")
            clear 1
            plugF <- getLine
            if plugF == "a"
                then do
                plugboard <- evaluate (makePlugboard plugA)
                clear 100
                handleMilFour rotorS reflector plugboard
                else if plugF == "b"
                    then do
                    plugboard <- evaluate (makePlugboard plugB)
                    clear 100
                    handleMilFour rotorS reflector plugboard
                else if plugF == "c"
                    then do
                    plugboard <- evaluate (makePlugboard plugC)
                    clear 100
                    handleMilFour rotorS reflector plugboard
                else if plugF == "q"
                    then do
                    quit
                else if plugF == "r"
                    then do
                    main
                else do
                    putStrLn(" Please input a valid plugboard")
                    clear 100
                    handleMilThree rotorS reflector

-- handles the fourth step in the military customization
handleMilFour rotorS reflector plugboard =
            do
            startScreen
            clear 5
            putStrLn(" Please enter the state of the three rotors (ex: ABC)")
            clear 1
            state <- (getLine)
            if state == "q"
                then do
                quit
                else if state == "r"
                    then do
                    main
                else do
                    rotorS <- evaluate (setStarting rotorS state)
                    handlecrypt rotorS plugboard reflector

-- handles the encryption of a string with given Enigma machine
handlecrypt rotorS plugboard reflector = do
                clear 100
                startScreen
                clear 5
                putStrLn(" Enter string to encrypt/decrypt")
                putStrLn("")
                text <- getLine
                clear 100
                startScreen
                clear 2
                result <- evaluate ((crypt rotorS plugboard reflector ( text)))
                putStrLn (" Input: " ++ map toUpper text ++ "\n\n" ++ " Output: " ++ result)
                clear 1
                putStrLn (" Would you like to quit (q) or restart (r)?")
                line <- getLine
                if line == "q"
                    then do
                    quit
                else if line == "r"
                    then do
                    main
                else do
                    clear 100

quit = do
       hSetBuffering stdin NoBuffering
       clear 100

while test action = do
  val <- test
  if val
    then do
    {action;while test action}

    else return ()

clear x = do
    ref <- newIORef 0
    while (test ref (<x))
        (
        do
        putStrLn("")
        incr ref
        )

incr ref = modifyIORef ref (+1)

test ref f = do { val <- readIORef ref; return (f val) }

startScreen = putStrLn  "  ___   _  _   ___     __   __  __     _   \n | __| | \\| | |_ _|  / __| |  \\/  |   /_\\  \n | _|  | .` |  | |  | (_ | | |\\/| |  / _ \\ \n |___| |_|\\_| |___|  \\___| |_|  |_| /_/ \\_\\\n                                                               "
