
module Main where

import Criterion.Main

fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

factorial 1 = 1
factorial n = n * factorial(n-1)

main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "11" $ whnf fib 11
               ],
  bgroup "factorial" [ bench "2" $ whnf factorial 2
  					 , bench "16" $ whnf factorial 16
  					 , bench "32" $ whnf factorial 32
  					 , bench "200" $ whnf factorial 200
  					 , bench "5000" $ whnf factorial 5000
  					 ]
  ]
