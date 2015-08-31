module Test where

import Probability

main :: IO ()
main = print $ showP test

flipP :: Prob Bool
flipP = choose [True, False]

test :: Prob Bool
test = normalize $ do
    flips <- sequence [flipP, flipP, flipP, flipP]
    return $ and flips
