
-- | Efficient combinatorial algorithms over multisets, including
--   generating all permutations, partitions, subsets, cycles, and
--   other combinatorial structures based on multisets.  Note that an
--   'Eq' or 'Ord' instance on the elements is /not/ required; the
--   algorithms are careful to keep track of which things are (by
--   construction) equal to which other things, so equality testing is
--   not needed.
module Math.Combinatorics.Multiset
       ( -- * The 'Multiset' type

         Count
       , Multiset(..)
       , emptyMS, singletonMS
       , consMS, (+:)

         -- ** Conversions
       , toList
       , fromList
       , fromListEq
       , fromDistinctList
       , fromCounts
       , getCounts
       , size

         -- ** Operations
       , disjUnion
       , disjUnions

         -- * Permutations

       , permutations
       , permutationsRLE

         -- * Partitions

       , Vec
       , vPartitions
       , partitions

         -- * Submultisets

       , splits
       , kSubsets

         -- * Cycles

       , cycles

         -- * Miscellaneous

       , sequenceMS

       ) where

import Data.List (group, sort)
import Control.Arrow (first, second, (&&&), (***))
import Data.Maybe (catMaybes)

type Count = Int

-- | A multiset is represented as a list of (element, count) pairs.
--   We maintain the invariants that the counts are always positive,
--   and no element ever appears more than once.
newtype Multiset a = MS { toCounts :: [(a, Count)] }
  deriving (Show)

-- | Construct a 'Multiset' from a list of (element, count) pairs.
--   Precondition: the counts must all be positive, and there must not
--   be any duplicate elements.
fromCounts :: [(a, Count)] -> Multiset a
fromCounts = MS

-- | Extract just the element counts from a multiset, forgetting the
--   elements.
getCounts :: Multiset a -> [Count]
getCounts (MS xs) = map snd xs

-- | Compute the total size of a multiset.
size :: Multiset a -> Int
size = sum . getCounts

liftMS :: ([(a, Count)] -> [(b, Count)]) -> Multiset a -> Multiset b
liftMS f (MS m) = MS (f m)

-- | A multiset with no values in it.
emptyMS :: Multiset a
emptyMS = MS []

-- | Create a multiset with only a single value in it.
singletonMS :: a -> Multiset a
singletonMS a = MS [(a,1)]

-- | Add an element with multiplicity to a multiset.  Precondition:
--   the new element is distinct from all elements already in the
--   multiset.
consMS :: (a, Count) -> Multiset a -> Multiset a
consMS e@(_,c) (MS m)
  | c > 0     = MS (e:m)
  | otherwise = MS m

-- | A convenient shorthand for 'consMS'.
(+:) :: (a, Count) -> Multiset a -> Multiset a
(+:) = consMS

instance Functor Multiset where
  fmap f = fromCounts . (map . first $ f) . toCounts

-- | Convert a multiset to a list.
toList :: Multiset a -> [a]
toList = expandCounts . toCounts

expandCounts :: [(a, Count)] -> [a]
expandCounts = concatMap (uncurry (flip replicate))

-- | Efficiently convert a list to a multiset, given an 'Ord' instance
--   for the elements.  This method is provided just for convenience.
--   you can also use 'fromListEq' with only an 'Eq' instance, or
--   construct 'Multiset's directly using 'fromCounts'.
fromList :: Ord a => [a] -> Multiset a
fromList = fromCounts . map (head &&& length) . group . sort

-- | Convert a list to a multiset, given an 'Eq' instance for the
--   elements.
fromListEq :: Eq a => [a] -> Multiset a
fromListEq = fromCounts . fromListEq'
  where fromListEq' []     = []
        fromListEq' (x:xs) = (x, 1 + count x xs) : fromListEq' (filter (/=x) xs)
        count x = length . filter (==x)

-- | Make a multiset with one copy of each element from a list of
--   distinct elements.
fromDistinctList :: [a] -> Multiset a
fromDistinctList = fromCounts . map (\x -> (x,1))

-- | Form the disjoint union of two multisets; i.e. we assume the two
--   multisets share no elements in common.
disjUnion :: Multiset a -> Multiset a -> Multiset a
disjUnion (MS xs) (MS ys) = MS (xs ++ ys)

-- | Form the disjoint union of a collection of multisets.  We assume
--   that the multisets all have distinct elements.
disjUnions :: [Multiset a] -> Multiset a
disjUnions = foldr disjUnion (MS [])

-- | In order to generate permutations of a multiset, we need to keep
--   track of the most recently used element in the permutation being
--   built, so that we don't use it again immediately.  The
--   'RMultiset' type (for \"restricted multiset\") records this
--   information, consisting of a multiset possibly paired with an
--   element (with multiplicity) which is also part of the multiset,
--   but should not be used at the beginning of permutations.
data RMultiset a = RMS (Maybe (a, Count)) [(a,Count)]
  deriving Show

-- | Convert a 'Multiset' to a 'RMultiset' (with no avoided element).
toRMS :: Multiset a -> RMultiset a
toRMS = RMS Nothing . toCounts

-- | Convert a 'RMultiset' to a 'Multiset'.
fromRMS :: RMultiset a -> Multiset a
fromRMS (RMS Nothing m)  = MS m
fromRMS (RMS (Just e) m) = MS (e:m)

-- | List all the distinct permutations of the elements of a
--   multiset.
--
--   For example, @permutations (fromList \"abb\") ==
--   [\"abb\",\"bba\",\"bab\"]@, whereas @Data.List.permutations
--   \"abb\" == [\"abb\",\"bab\",\"bba\",\"bba\",\"bab\",\"abb\"]@.
--   This function is equivalent to, but /much/ more efficient than,
--   @nub . Data.List.permutations@, and even works when the elements
--   have no 'Eq' instance.
--
--   Note that this is a specialized version of 'permutationsRLE',
--   where each run has been expanded via 'replicate'.
permutations :: Multiset a -> [[a]]
permutations = map expandCounts . permutationsRLE

-- | List all the distinct permutations of the elements of a multiset,
--   with each permutation run-length encoded. (Note that the
--   run-length encoding is a natural byproduct of the algorithm used,
--   not a separate postprocessing step.)
--
--   For example, @permutationsRLE [('a',1), ('b',2)] ==
--   [[('a',1),('b',2)],[('b',2),('a',1)],[('b',1),('a',1),('b',1)]]@.
--
--   (Note that although the output type is newtype-equivalent to
--   @[Multiset a]@, we don't call it that since the output may
--   violate the 'Multiset' invariant that no element should appear
--   more than once.  And indeed, morally this function does not
--   output multisets at all.)
permutationsRLE :: Multiset a -> [[(a,Count)]]
permutationsRLE (MS []) = [[]]
permutationsRLE m       = permutationsRLE' (toRMS m)

-- | List all the (run-length encoded) distinct permutations of the
--   elements of a multiset which do not start with the element to
--   avoid (if any).
permutationsRLE' :: RMultiset a -> [[(a,Count)]]

-- If only one element is left, there's only one permutation.
permutationsRLE' (RMS Nothing [(x,n)]) = [[(x,n)]]

-- Otherwise, select an element+multiplicity in all possible ways, and
-- concatenate the elements to all possible permutations of the
-- remaining multiset.
permutationsRLE' m  = [ e : p
                      | (e, m') <- selectRMS m
                      , p       <- permutationsRLE' m'
                      ]

-- | Select an element + multiplicity from a multiset in all possible
--   ways, appropriately keeping track of elements to avoid at the
--   start of permutations.
selectRMS :: RMultiset a -> [((a, Count), RMultiset a)]

-- No elements to select.
selectRMS (RMS _ [])            = []

-- Selecting from a multiset with n copies of x, avoiding e:
selectRMS (RMS e ((x,n) : ms))  =

  -- If we select all n copies of x, there are no copies of x left to avoid;
  -- stick e (if it exists) back into the remaining multiset.
  ((x,n), RMS Nothing (maybe ms (:ms) e)) :

  -- We can also select any number of copies of x from (n-1) down to 1; in each case,
  -- we avoid the remaining copies of x and put e back into the returned multiset.
  [ ( (x,k), RMS (Just (x,n-k))
                 (maybe ms (:ms) e) )
    | k <- [n-1, n-2 .. 1]
  ] ++

  -- Finally, we can recursively choose something other than x.
  map (second (consRMS (x,n))) (selectRMS (RMS e ms))

consRMS :: (a, Count) -> RMultiset a -> RMultiset a
consRMS x (RMS e m) = RMS e (x:m)


-- Some QuickCheck properties.  Of course, due to combinatorial
-- explosion these are of limited utility!
-- newtype ArbCount = ArbCount Int
--   deriving (Eq, Show, Num, Real, Enum, Ord, Integral)

-- instance Arbitrary Count where
--   arbitrary = elements (map ArbCount [1..3])

-- prop_perms_distinct :: Multiset Char ArbCount -> Bool
-- prop_perms_distinct m = length ps == length (nub ps)
--   where ps = permutations m

-- prop_perms_are_perms :: Multiset Char ArbCount -> Bool
-- prop_perms_are_perms m = all ((==l') . sort) (permutations m)
--   where l' = sort (toList m)

---------------------
-- Partitions
---------------------

-- | Element count vector.
type Vec = [Count]

-- | Componentwise comparison of count vectors.
(<|=) :: Vec -> Vec -> Bool
xs <|= ys = and $ zipWith (<=) xs ys

-- | 'vZero v' produces a zero vector of the same length as @v@.
vZero :: Vec -> Vec
vZero = map (const 0)

-- | Test for the zero vector.
vIsZero :: Vec -> Bool
vIsZero = all (==0)

-- | Do vector arithmetic componentwise.
(.+.), (.-.) :: Vec -> Vec -> Vec
(.+.) = zipWith (+)
(.-.) = zipWith (-)

-- | Multiply a count vector by a scalar.
(*.) :: Count -> Vec -> Vec
(*.) n = map (n*)

-- | 'v1 `vDiv` v2' is the largest scalar multiple of 'v2' which is
--   elementwise less than or equal to 'v1'.
vDiv :: Vec -> Vec -> Count
vDiv v1 v2 = minimum . catMaybes $ zipWith zdiv v1 v2
  where zdiv _ 0 = Nothing
        zdiv x y = Just $ x `div` y

-- | 'vInc within v' lexicographically increments 'v' with respect to
--   'within'.  For example, @vInc [2,3,5] [1,3,4] == [1,3,5]@, and
--   @vInc [2,3,5] [1,3,5] == [2,0,0]@.
vInc :: Vec -> Vec -> Vec
vInc lim v = reverse (vInc' (reverse lim) (reverse v))
  where vInc' _ []          = []
        vInc' [] (x:xs)     = x+1 : xs
        vInc' (l:ls) (x:xs) | x < l     = x+1 : xs
                            | otherwise = 0 : vInc' ls xs

-- | Generate all vector partitions, representing each partition as a
--   multiset of vectors.
--
--   This code is a slight generalization of the code published in
--
--     Brent Yorgey. \"Generating Multiset Partitions\". In: The
--     Monad.Reader, Issue 8, September 2007.
--     <http://www.haskell.org/sitewiki/images/d/dd/TMR-Issue8.pdf>
--
--   See that article for a detailed discussion of the code and how it works.
vPartitions :: Vec -> [Multiset Vec]
vPartitions v = vPart v (vZero v) where
  vPart v _ | vIsZero v = [MS []]
  vPart v vL
    | v <= vL   = []
    | otherwise = MS [(v,1)]
                : [ (v',k) +: p' | v' <- withinFromTo v (vHalf v) (vInc v vL)
                                 , k  <- [1 .. (v `vDiv` v')]
                                 , p' <- vPart (v .-. (k *. v')) v' ]

-- | 'vHalf v' computes the \"lexicographic half\" of 'v', that is,
--   the vector which is the middle element (biased towards the end)
--   in a lexicographically decreasing list of all the vectors
--   elementwise no greater than 'v'.
vHalf :: Vec -> Vec
vHalf [] = []
vHalf (x:xs) | (even x) = (x `div` 2) : vHalf xs
             | otherwise = (x `div` 2) : xs

downFrom n = [n,(n-1)..0]

-- | 'within m' generates a lexicographically decreasing list of
--   vectors elementwise no greater than 'm'.
within :: Vec -> [Vec]
within = sequence . map downFrom

-- | Clip one vector against another.
clip :: Vec -> Vec -> Vec
clip = zipWith min

-- | 'withinFromTo m s e' efficiently generates a lexicographically
--   decreasing list of vectors which are elementwise no greater than
--   'm' and lexicographically between 's' and 'e'.
withinFromTo :: Vec -> Vec -> Vec -> [Vec]
withinFromTo m s e | not (s <|= m) = withinFromTo m (clip m s) e
withinFromTo m s e | e > s = []
withinFromTo m s e = wFT m s e True True
  where
    wFT [] _ _ _ _ = [[]]
    wFT (m:ms) (s:ss) (e:es) useS useE =
        let start = if useS then s else m
            end   = if useE then e else 0
        in
          [x:xs | x <- [start,(start-1)..end],
                  let useS' = useS && x==s,
                  let useE' = useE && x==e,
                  xs <- wFT ms ss es useS' useE' ]

-- | Efficiently generate all distinct multiset partitions.  Note that
--   each partition is represented as a multiset of parts (each of
--   which is a multiset) in order to properly reflect the fact that
--   some parts may occur multiple times.
partitions :: Multiset a -> [Multiset (Multiset a)]
partitions (MS []) = [MS []]
partitions (MS m)  = (map . fmap) (combine elts) $ vPartitions counts
  where (elts, counts) = unzip m
        combine es cs  = MS . filter ((/=0) . snd) $ zip es cs

-- | Generate all splittings of a multiset into two submultisets,
--   i.e. all size-two partitions.
splits :: Multiset a -> [(Multiset a, Multiset a)]
splits (MS [])        = [(MS [], MS [])]
splits (MS ((x,n):m)) =
  for [0..n] $ \k ->
    map (addElt x k *** addElt x (n-k)) (splits (MS m))

-- | Generate all size-k submultisets.
kSubsets :: Count -> Multiset a -> [Multiset a]
kSubsets 0 _              = [MS []]
kSubsets _ (MS [])        = []
kSubsets k (MS ((x,n):m)) =
  for [0 .. min k n] $ \j ->
    map (addElt x j) (kSubsets (k - j) (MS m))

for = flip concatMap

addElt _ 0 = id
addElt x k = ((x,k) +:)

-- | Generate all distinct cycles, aka necklaces, with elements taken
--   from a multiset.  See J. Sawada, \"A fast algorithm to generate
--   necklaces with fixed content\", J. Theor. Comput. Sci. 301 (2003)
--   pp. 477-489.
--
--   Given the ordering on the elements of the multiset based on their
--   position in the multiset representation (with \"smaller\"
--   elements first), in @map reverse (cycles m)@, each generated
--   cycle is lexicographically smallest among all its cyclic shifts,
--   and furthermore, the cycles occur in reverse lexicographic
--   order. (It's simply more convenient/efficient to generate the
--   cycles reversed in this way, and of course we get the same set of
--   cycles either way.)
--
--   For example, @cycles (fromList \"aabbc\") ==
--   [\"cabba\",\"bcaba\",\"cbaba\",\"bbcaa\",\"bcbaa\",\"cbbaa\"]@.
cycles :: Multiset a -> [[a]]
cycles (MS [])         = []   -- no such thing as an empty cycle
cycles m@(MS ((x1,n1):xs))
  | n1 == 1    = (cycles' n 2 1 [(0,x1)] (reverse $ zip [1..] xs))
  | otherwise =  (cycles' n 2 1 [(0,x1)] (reverse $ zip [0..] ((x1,n1-1):xs)))
  where n = sum . getCounts $ m

-- | The first parameter is the length of the necklaces being
--   generated.  The second parameter @p@ is the length of the longest
--   prefix of @pre@ which is a Lyndon word, i.e. an aperiodic
--   necklace.  @pre@ is the current (reversed) prefix of the
--   necklaces being generated.
cycles' :: Int -> Int -> Int -> [(Int, a)] -> [(Int, (a,Count))] -> [[a]]
cycles' n t p pre [] | n `mod` p == 0 = [map snd pre]
                     | otherwise      = []

cycles' n t p pre xs =
  (takeWhile ((>=atp) . fst) xs) >>= \(j, (xj,nj)) ->
    cycles' n (t+1) (if j == atp then p else t)
      ((j,xj):pre)
      (remove j xs)
  where atp = fst $ pre !! (p - 1)

remove j [] = []
remove j (x@(j',(xj,nj)):xs)
  | j == j' && nj == 1 = xs
  | j == j'            = (j',(xj,nj-1)):xs
  | otherwise          = x:remove j xs

-- | Take a multiset of lists, and select one element from each list
--   in every possible combination to form a list of multisets.  We
--   assume that all the list elements are distinct.
sequenceMS :: Multiset [a] -> [Multiset a]
sequenceMS = map disjUnions
           . sequence
           . map (\(xs, n) -> kSubsets n (MS $ uncollate (xs, n)))
           . toCounts

uncollate :: ([a], Count) -> [(a, Count)]
uncollate (xs, n) = map (\x -> (x,n)) xs

