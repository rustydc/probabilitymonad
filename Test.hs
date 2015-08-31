module Test where

import           Probability
import Text.Printf

main :: IO ()
main = printf "%f%%" $ 100 * chance test

roulette :: Prob Integer
roulette = choose [0..36]

flipP :: Prob Bool
flipP = choose [True, False]

test :: Prob Bool
test = normalize $ do
    spin1 <- roulette
    spin2 <- roulette
    flip1 <- flipP
    return $ spin1 == 13 && spin2 == 13 && flip1
