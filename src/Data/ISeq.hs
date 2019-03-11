module Data.ISeq where

import Prelude hiding ( seq )

import Data.List
import Util

iNil :: ISeq
iStr :: String -> ISeq
iAppend :: ISeq -> ISeq -> ISeq
iNewline :: ISeq
iIndent :: ISeq -> ISeq
iDisplay :: ISeq -> String

-- |
-- Following 'iConcat' and 'iInterleave' implementations are exercise 1.2
iConcat :: [ISeq] -> ISeq
iConcat = foldl iAppend iNil

iInterleave :: ISeq -> [ISeq] -> ISeq
iInterleave _ [] = iNil
iInterleave sep (seq : seqs) = foldl (\acc s -> acc `iAppend` sep `iAppend` s) seq seqs

-- |
-- Following 'iPrecParen' implementation is a part of exercise 1.8
iPrecParen :: Int -> Int -> ISeq -> ISeq
iPrecParen contextPrec currentPrec seq
  | contextPrec > currentPrec = iConcat [ iStr "(", seq, iStr ")" ]
  | otherwise = seq

-- |
-- Following definitions do not support indenting.
{-
data ISeq
  = INil
  | IStr String
  | IAppend ISeq ISeq
-}

iNil = INil
-- |
-- Before exercise 1.7
{-
iStr = IStr
-}
-- |
-- Following implementation of 'iStr' is exercise 1.7
iStr = iConcat . intersperse INewline . map IStr . lines
-- |
-- Before exercise 1.5
{-
iAppend = IAppend
-}
-- |
-- Following implementation of 'iAppend' is exercise 1.5
iAppend INil INil = INil
iAppend INil seq2 = seq2
iAppend seq1 INil = seq1
iAppend seq1 seq2 = IAppend seq1 seq2
-- |
-- Following definitions do not support indenting.
{-
iIndent seq = seq
iNewline = IStr "\n"

flatten :: [ISeq] -> String

iDisplay = flatten . return

flatten [] = ""
flatten (INil : seqs) = flatten seqs
flatten (IStr s : seqs) = s ++ flatten seqs
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)
-}

data ISeq
  = INil
  | IStr String
  | IAppend ISeq ISeq
  | IIndent ISeq
  | INewline

iIndent = IIndent
iNewline = INewline

flatten :: Int -> [(ISeq, Int)] -> String

iDisplay seq = flatten 0 [(seq, 0)]

flatten _ ((INewline, indent) : seqs)
  = '\n' : space indent ++ flatten indent seqs
flatten col ((IIndent seq, _) : seqs)
  = flatten col ((seq, col) : seqs)
-- |
-- Following patterns for 'flatten' are parts of exercise 1.6
flatten col ((IAppend seq1 seq2, indent) : seqs)
  = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((IStr s, _) : seqs) = s ++ flatten (col + length s) seqs
flatten col ((INil, _) : seqs) = flatten col seqs
flatten _ [] = ""

iNum :: Int -> ISeq
iNum = iStr . show

iFWNum :: Int -> Int -> ISeq
iFWNum width n
  = iStr (space (width - length digits) ++ digits)
  where
    digits = show n

-- |
-- This function is named after the similar function of the Miranda
iLayn :: [ISeq] -> ISeq
iLayn seqs
  = iConcat (zipWith layItem [1..] seqs)
  where
    layItem n seq
      = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]
