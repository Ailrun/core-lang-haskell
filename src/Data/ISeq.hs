{-# LANGUAGE CPP #-}
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

#if __CLH_EXERCISE_1__ >= 2
iConcat :: [ISeq] -> ISeq
iConcat = foldl iAppend iNil

iInterleave :: ISeq -> [ISeq] -> ISeq
iInterleave _ [] = iNil
iInterleave sep (seq : seqs) = foldl (\s acc -> s `iAppend` sep `iAppend` acc) seq seqs

#if __CLH_EXERCISE_1__ < 6
data ISeq
  = INil
  | IStr String
  | IAppend ISeq ISeq
#endif

iNil = INil

#if __CLH_EXERCISE_1__ < 7
iStr = IStr
#endif

#if __CLH_EXERCISE_1__ < 5
iAppend = IAppend
#endif

#if __CLH_EXERCISE_1__ >= 5
iAppend INil INil = INil
iAppend INil seq2 = seq2
iAppend seq1 INil = seq1
iAppend seq1 seq2 = IAppend seq1 seq2

#if __CLH_EXERCISE_1__ < 6
iIndent seq = seq
iNewline = IStr "\n"

flatten :: [ISeq] -> String

iDisplay = flatten . return

flatten [] = ""
flatten (INil : seqs) = flatten seqs
flatten (IStr s : seqs) = s ++ flatten seqs
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)
#endif

#if __CLH_EXERCISE_1__ >= 6
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
flatten col ((IAppend seq1 seq2, indent) : seqs)
  = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((IStr s, _) : seqs) = s ++ flatten (col + length s) seqs
flatten col ((INil, _) : seqs) = flatten col seqs
flatten _ [] = ""

#if __CLH_EXERCISE_1__ >= 7
iStr = iConcat . intersperse INewline . map IStr . lines
#endif

#if __CLH_EXERCISE_1__ >= 8
iPrecParen :: Int -> Int -> ISeq -> ISeq
iPrecParen contextPrec currentPrec seq
  | contextPrec > currentPrec = iConcat [ iStr "(", seq, iStr ")" ]
  | otherwise = seq

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
#endif
#endif
#endif
#endif
