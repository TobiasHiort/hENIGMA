module Cryption(crypt, runEnTests) where

import Mechanics
import Data.Char
import Test.HUnit

------------------------------------------------------------------------
-- CRYPTOR FUNCTIONS
-- all functions involved in en- or decrypting.
------------------------------------------------------------------------

{- cryptRight rots char
   PURPOSE:  encrypt a character through a series of Rotors from left to right.
   PRE:  char is uppercase.
   POST: char encrypted from left to right through rots.
   EXAMPLES:    cryptRight [] 'A' = 'A'
                cryptRight r (where r is arbitrary rotor list) 'A' = 'J'
-}
cryptRight :: [Rotor] -> Char -> Char
cryptRight (((b,(a,c)):ys):xs) z
    | z == b && xs /= [] = cryptRight xs a
    | z == b = a
    | otherwise = cryptRight (ys:xs) z
cryptRight [] z = z

{- cryptLeft rots char
   PURPOSE:  encrypt a character through a series of rotors from right to left.
   PRE:  char is uppercase.
   POST: char encrypted from right to left through rots.
   EXAMPLES:    cryptRight [] 'A' = 'A'
                cryptRight r (where r is arbitrary rotor list) 'J' = 'A'
-}
cryptLeft :: [Rotor] -> Char -> Char
cryptLeft (((b,(a,c)):ys):xs) z
    | z == a && xs /= [] = cryptLeft xs b
    | z == a = b
    | otherwise = cryptLeft (ys:xs) z
cryptLeft [] z = z

{- crypt rots plug ref str
   PURPOSE:  en- or decrypts a string through plugboard, rotors and reflector.
   PRE: rotors rots and reflector ref can not be empty.
   POST: string str en- or decrypted through plugboard plug, rotors rots and reflector ref.
   EXAMPLES:    (rots, plug, ref are valid rotor list, plugboard and reflector)
                encrypt rots  plug ref "HEJSAN" = "SSURFF"
                encrypt rots  plug ref "" = ""
-}
crypt :: [Rotor] -> Plugboard -> Reflector -> String -> String
crypt (((b,(a,c)):ys):xs) p ref (y:yx) = do
    y <- plug p [(toUpper y)]
    if yx /= []
     then
     (plug p [((cryptRight (reverse (((b,(a,c)):ys):xs))
        (reflect ref (cryptLeft (((b,(a,c)):ys):xs) y)) ))] ++
        (crypt (rotate (((b,(a,c)):ys):xs)) p ref yx))
     else
     plug p [(cryptRight (reverse (((b,(a,c)):ys):xs)) (reflect
        ref (cryptLeft (((b,(a,c)):ys):xs) y)))]
crypt _ _ _ s = s

------------------------------------------------------------------------
-- TEST FUNCTIONS
-- all functions involved in testing this module.
------------------------------------------------------------------------

testEn1 = TestCase (assertEqual "For cryptRight [] 'A', " 'A' (cryptRight [] 'A'))
testEn2 = TestCase (assertEqual "For cryptRight (fst getPublic) 'A', " 'D' (cryptRight (fst getPublic) 'A'))
testEn3 = TestCase (assertEqual "For cryptLeft [] 'A', " 'A' (cryptLeft [] 'A'))
testEn4 = TestCase (assertEqual "For cryptLeft (fst getPublic) 'A', " 'G' (cryptLeft (fst getPublic) 'A'))
testEn5 = TestCase (assertEqual "For crypt (fst getPublic) (fst (snd getPublic)) (snd (snd getPublic)) \"HEJSAN\", " "BMDIZX" (crypt (fst getPublic) (fst (snd getPublic)) (snd (snd getPublic)) "HEJSAN"))
testEn6 = TestCase (assertEqual "For crypt (fst getPublic) (fst (snd getPublic)) (snd (snd getPublic)) \"HeJsAn\", " "BMDIZX" (crypt (fst getPublic) (fst (snd getPublic)) (snd (snd getPublic)) "HeJsAn"))
testEn7 = TestCase (assertEqual "For crypt (fst getPublic) (fst (snd getPublic)) (snd (snd getPublic)) \"\", " "" (crypt (fst getPublic) (fst (snd getPublic)) (snd (snd getPublic)) ""))

runEnTests = runTestTT $ TestList [testEn1, testEn2, testEn3, testEn4, testEn5, testEn6, testEn7]
