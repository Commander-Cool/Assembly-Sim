-- Simulate a very simple CPU

module CPU where

import Data.Array

data Instruction  = Load           RegisterNumber       -- put value here
                                   RegisterNumber       -- from address which is sum of this register value
                                   RegisterNumber       -- and this register value

                  | Store          RegisterNumber       -- store value in this register
                                   RegisterNumber       -- to address which is sum of this register value
                                   RegisterNumber       --   and this register value

                  | LoadImmediate  RegisterNumber       -- put value here
                                   RegisterValue        -- the value

                  | Add            RegisterNumber       -- put result here
                                   RegisterNumber       -- first thing to add
                                   RegisterNumber       -- second thing to multiply

                  | Multiply       RegisterNumber       -- put result here
                                   RegisterNumber       -- first thing to multiply
                                   RegisterNumber       -- second thing to multiply

                  | And            RegisterNumber       -- put result here
                                   RegisterNumber       -- first thing to and
                                   RegisterNumber       -- second thing to and

                  | Or             RegisterNumber       -- put result here
                                   RegisterNumber       -- first thing to or
                                   RegisterNumber       -- second thing to or

                  | Not            RegisterNumber       -- put result here
                                   RegisterNumber       -- reverse bits from here

                  | Rotate         RegisterNumber       -- put result here
                                   RegisterNumber       -- value to rotate
                                   Int                  -- rotate bits (left is positive)

                  | Compare        RegisterNumber       -- compare the value in this register
                                   RegisterNumber       -- to the value in this register (result goes in CPU State)

                  | Branch         [ComparisonResult]   -- results (maybe all) which will cause branch
                                   RegisterNumber       -- instruction to branch to if true

                  | Halt
      deriving (Show,Eq)

type Program = Int -> Instruction

type Data = Array Int Int
type Code = [(Integer,Instruction)]

type RegisterNumber = Int
type RegisterValue = Int
type ComparisonResult = Ordering
