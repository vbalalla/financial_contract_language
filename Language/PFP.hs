import Numeric.Probability

type Die = Int

die :: Dist Die
die = uniform [1..6]
