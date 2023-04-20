module Instruction where

data Instruction = Next | Prev | Inc | Dec | Read | Write | Loop [Instruction] 
    deriving (Show)
