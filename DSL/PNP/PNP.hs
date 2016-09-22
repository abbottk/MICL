module PNP where

import Primitive


--
-- * Programs as plans
--  ____________________
-- | DO:                |
-- |                    |
-- |____________________|
--           || ______________
--           || ------------- |
--  _________\/_________     ||_____________________
-- | DO:                |    |                      |
-- |                    |    |  INTERSPERSE "OFFER" |
-- |____________________|    | _____________________|
--           || _____________||
--           || --------------
--  _________\/_________
-- | DO:                |
-- |                    |
-- |____________________|
--

-- | Plan constructors
data Plan = Block Explain [Action]
          deriving(Show)

type Explain = String

data Action = Move Signal
            | Exception Signal OpMode
            | Prompt Signal Task
            deriving(Show)

type Task = Either Waypoint Instruction

type Waypoint = Location
type Location = (North,East,Down)
type North = Double
type East = Double
type Down = Double

type Instruction = String

type OpMode = (Agent,Mode)

data Agent = Human
           | Computer
           deriving(Show)

data Mode = Recovery
          | Normal
          | Return
          | Wait
          deriving(Show)

data Signal = Signal { order :: Int,
                       agent :: Agent,
                       enable :: Bool,
                       roll :: Float,
                       pitch :: Float,
                       gaz :: Float,
                       yaw :: Float
                     }
            deriving(Show)
