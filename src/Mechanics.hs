module Mechanics(makeRotor, getMilitary, getPublic, makeReflector,
                 makePlugboard, rotateRotor, plug, reflect, refA,runMeTests,
                 refB, refC, plugA, plugB, plugC, setStarting, rotate,
                 Rotor, Reflector, Plugboard, RState) where

import Data.Char
import Test.HUnit

------------------------------------------------------------------------
-- DATATYPES
------------------------------------------------------------------------

{- REPRESENTATION CONVENTION:   Plugboard represents a list of pair of characters,
                                which are connected to each other.
   REPRESENTATION INVARIANT:    Characters must be A-Z, where any one char can
                                only be connected to one other character.
-}
type Plugboard = [(Char, Char)]

{- REPRESENTATION CONVENTION:   Rotor represents a rotor wheel where the
                                two characters are connected and the Boolean
                                represents if there is a notch or not.
   REPRESENTATION INVARIANT:    All characters in the alphabet(A-Z) must be
                                connected to one and only one output.
-}
type Rotor = [(Char, (Char, Bool))]

{- REPRESENTATION CONVENTION:   Reflector represents a list of pair of chars,
                                which are connected to each other.
   REPRESENTATION INVARIANT:    All characters in the alphabet(A-Z) must be
                                connected to one and only one output.
-}
type Reflector = [(Char, Char)]

{- REPRESENTATION CONVENTION:   RState represents the state that the rotors
                                are set to.
   REPRESENTATION INVARIANT:    Chars must be A-Z.
-}
type RState = [Char]

------------------------------------------------------------------------
-- ROTORS, REFLECTORS AND PLUGBOARDS
------------------------------------------------------------------------

alpha       =  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Rotors
mOne        = ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",'Q')
mTwo        = ("AJDKSIRUXBLHWTMCQGZNPYFVOE",'E')
mThree      = ("BDFHJLCPRTXVZNYEIWGAKMUSQO",'V')
mFour       = ("ESOVPZJAYQUIRHXLNFTGKDCMWB",'J')
mFive       = ("VZBRGITYUPSDNHLXAWMJQOFECK",'Z')

-- Reflectors
refA        =  "EJMZALYXVBWFCRQUONTSPIKHGD"
refB        =  "YRUHQSLDPXNGOKMIEBFZCWVJAT"
refC        =  "FVPJIAOYEDRZXWGCTKUQSBNMHL"

-- Plugboards
plugA       =  "ENKQAUYWJICOPBLMDXZV"
plugB       =  "VEHMLFCWZA"
plugC       =  "NTKV"
emptyPlug   =  ""

------------------------------------------------------------------------
-- MAKE FUNCTIONS
-- all the functions involved in making parts of the enigma machine.
------------------------------------------------------------------------

-- makeRotor creates a rotor from two strings and a char.
makeRotor :: String -> String -> Char -> Rotor
makeRotor (a:as) (b:bs) notch
    | as /= [] && notch /= a = (b,(a,False)):(makeRotor as bs notch)
    | as /= [] = (b,(a,True)):(makeRotor as bs notch)
    | notch == a = [(b,(a,True))]
    | otherwise = [(b,(a,False))]

-- makeReflector creates a reflector from a string and the alphabet.
makeReflector :: String -> Reflector
makeReflector (b:bs) = makeReflectorAux alpha (b:bs)

makeReflectorAux (a:ax) (b:bs)
    | ax /= [] = (b,a):(makeReflectorAux ax bs)
    | otherwise = [(b,a)]

-- makePlugboard creates a plugboard from a string.
makePlugboard :: String -> Plugboard
makePlugboard (a:b:ax)
    | [b] /=  [] && length ax > 1 = (a, b):(makePlugboard ax)
    | [b] /=  [] = [(a, b)]
    | otherwise = []
makePlugboard [] = []

------------------------------------------------------------------------
-- GET FUNCTIONS
-- all the functions involved in getting parts of the enigma machine.
------------------------------------------------------------------------

-- getMilitary gets 3 of the 5 military standars rotors.
getMilitary :: [Int] -> [Rotor]
getMilitary xs =    getMilitaryAux xs [makeRotor alpha (fst mOne) (snd mOne),
                    makeRotor alpha (fst mTwo) (snd mTwo),
                    makeRotor alpha (fst mThree) (snd mThree),
                    makeRotor alpha (fst mFour) (snd mFour),
                    makeRotor alpha (fst mFive) (snd mFive)]

getMilitaryAux :: [Int] -> [Rotor] -> [Rotor]
getMilitaryAux (x:xs) ys
    |xs /= [] = ((ys!!(x-1)):(getMilitaryAux xs ys))
    |otherwise = [ys!!(x-1)]

-- getPublic gets the standard public rotors, plugboard and reflector.
getPublic :: ([Rotor],(Plugboard,Reflector))
getPublic = ([makeRotor alpha (fst mOne) (snd mOne),makeRotor alpha
            (fst mTwo) (snd mTwo),makeRotor alpha (fst mThree)
            (snd mThree)],(makePlugboard emptyPlug,makeReflector refA))

------------------------------------------------------------------------
-- MECHANISM FUNCTIONS
-- all the functions involved with the mechanics of the enigma machine.
------------------------------------------------------------------------

{- setStarting rots rs
   PURPOSE:  sets the starting states of rotors in a rotor list.
   PRE:  true.
   POST: rotors in rots set to the positions rs.
   EXAMPLES:    (rots, rotsn are arbitrary rotor lists)
                setStarting [] "ABC" = []
                setStarting rots "" = rotsn (rots set to standard state AAA)
                setStarting rots "ABC" = rotsn (rots set to the state ABC)
-}
setStarting :: [Rotor] -> RState -> [Rotor]
setStarting (((b,(a,c)):ys):xs) (s:st)
    | a /= (toUpper s) && xs /= [] = setStarting ((rotateRotor
               ((b,(a,c)):ys)):xs) (s:st)
    | a == (toUpper s) && xs /= [] = ((b,(a,c)):ys) :
               setStarting xs st
    | a /= (toUpper s) = setStarting
               ((rotateRotor ((b,(a,c)):ys)):xs) (s:st)
    | a == (toUpper s) = [((b,(a,c)):ys)]
    | otherwise = []
setStarting x [] = setStarting x "AAA"
setStarting [] z = []
setStarting _ _ = []

{- rotate rots
   PURPOSE:  rotates the first rotor in a list of rotors, if it turns
             over a notch it rotates the next rotor in the list.
   PRE:  true.
   POST: rots rotated one time.
   EXAMPLES:    (rots, rotsn are arbitrary rotor lists)
                rotate [] = []
                rotate rots = rotsn (rotsn is rots going from state AAA
                                     to AAB)
                rotate rots = rotsn (rotsn is rots goign from state AAQ
                                     to ABR. Q is notch)
-}
rotate :: [Rotor] -> [Rotor]
rotate (((b,(a,c)):ys):xs)
    |c == True = (rotateRotor ((b,(a,c)):ys)):rotate xs
    |otherwise = (rotateRotor ((b,(a,c)):ys)):xs
rotate _ = []

{- rotateRotor rot
   PURPOSE:  rotates a rotor one step.
   PRE:  true.
   POST: rot rotated one step.
   EXAMPLES:    (rot,rRot are arbitrary rotors)
                rotateRotor [] = []
                rotateRotor rot = rRot (where rRot is rot rotated one step)
-}
rotateRotor :: Rotor -> Rotor
rotateRotor ((b,(a,c)):xs)
    | xs /= [] = (b, snd (head xs)) : (rotateRotorAux xs (a,c))
    | otherwise = [(b, (a,c))]
rotateRotor [] = []

rotateRotorAux :: Rotor -> (Char, Bool) -> Rotor
rotateRotorAux ((b,(a,c)):xs) z
    | xs /= [] = (b, snd (head xs)) : (rotateRotorAux xs z)
    | otherwise = [(b, z)]

{- plug pb cl
   PURPOSE:  plugs a list of one character through a plugboard.
   PRE:  length cl == 1.
   POST: cl plugged through pb.
   EXAMPLES:    (pb is an arbitrary plugboard)
                plug [] "A" =  "A"
                plug pb "A" =  "B"
                plug pb "B" =  "A"
-}
plug :: Plugboard -> [Char] -> [Char]
plug ((a,b):px) c
    |c == [a] = [b]
    |c == [b] = [a]
    |px /= [] = plug px c
    |otherwise = c
plug _ c = c

{- reflect ref char
   PURPOSE:  reflects a character in a reflector.
   PRE:  char exists.
   POST: char reflected in reflector ref.
   EXAMPLES:    (ref is an arbitrary reflector)
                reflect [] 'A' = 'A'
                reflect ref 'A' = 'B'
                reflect ref 'B' = 'A'
-}
reflect :: Reflector -> Char -> Char
reflect ((a,b):xs) c
    | a == c = b
    | xs /= [] = reflect xs c
reflect [] c = c

------------------------------------------------------------------------
-- TEST FUNCTIONS
-- all functions involved in testing this module.
------------------------------------------------------------------------

testMe1 = TestCase (assertEqual "For setStarting [[('B',('A',False)),('C',('B',False)),('D',('C',True))]] \"C\", " [[('B',('C',True)),('C',('A',False)),('D',('B',False))]] (setStarting [[('B',('A',False)),('C',('B',False)),('D',('C',True))]] "C"))
testMe2 = TestCase (assertEqual "For setStarting [] \"ABC\", " [] (setStarting [] ""))
testMe3 = TestCase (assertEqual "For setStarting [[('B',('A',False)),('C',('B',False)),('D',('C',True))]], " [[('B',('B',False)),('C',('C',True)),('D',('A',False))]] (rotate [[('B',('A',False)),('C',('B',False)),('D',('C',True))]]))
testMe4 = TestCase (assertEqual "For setStarting [[('B',('A',True)),('C',('B',False)),('D',('C',False))],[('B',('A',False)),('C',('B',False)),('D',('C',True))]], " [[('B',('B',False)),('C',('C',False)),('D',('A',True))],[('B',('B',False)),('C',('C',True)),('D',('A',False))]] (rotate [[('B',('A',True)),('C',('B',False)),('D',('C',False))],[('B',('A',False)),('C',('B',False)),('D',('C',True))]]))
testMe5 = TestCase (assertEqual "For rotateRotor [('B',('A',True)),('C',('B',False)),('D',('C',False))], " [('B',('B',False)),('C',('C',False)),('D',('A',True))] (rotateRotor [('B',('A',True)),('C',('B',False)),('D',('C',False))]))
testMe6 = TestCase (assertEqual "For rotateRotor [], " [] (rotateRotor []))
testMe7 = TestCase (assertEqual "For plug [('Z','X'),('A','B')] \"A\", " "B" (plug [('Z','X'),('A','B')] "A"))
testMe8 = TestCase (assertEqual "For plug [] \"A\", " "A" (plug [] "A"))
testMe9 = TestCase (assertEqual "For reflect [('A','B')] \'A\', " 'B' (reflect [('A','B')] 'A'))
testMe10 = TestCase (assertEqual "For reflect [] \"A\", " 'A' (reflect [] 'A'))

runMeTests = runTestTT $ TestList [testMe1, testMe2, testMe3, testMe4, testMe5, testMe6, testMe7, testMe8, testMe9, testMe10]


