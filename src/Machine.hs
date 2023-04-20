module Machine where

import qualified Control.Lens as Lens

data Machine = Machine {
    tape :: [Int],
    pointer :: Int
}

init :: Machine
init = Machine (repeat 0) 0

inc :: Machine -> Machine
inc m = set (get m + 1) m

dec :: Machine -> Machine
dec m = set (get m - 1) m

prev :: Machine -> Machine
prev m = m { pointer = pointer m - 1 }

next :: Machine -> Machine
next m = m { pointer = pointer m + 1 }

get :: Machine -> Int
get = (!!) <$> tape <*> pointer

set :: Int -> Machine -> Machine
set v m = m { tape = Lens.set (Lens.element i) v (tape m) }
    where i = pointer m
