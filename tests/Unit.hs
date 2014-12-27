module Main where

import Test.HUnit


factorial 1 = 1
factorial n = n * factorial (n-1)

testFiveFactorial = TestCase $ assertEqual 
  "Testing 5!"
  (1*2*3*4*5) 
  (factorial 5) 

main = runTestTT $ TestList[
	testFiveFactorial,
	testFiveFactorial]


