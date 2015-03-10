import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Ord as Ord
import System.Environment
import System.IO
import System.Random
import Data.CSV
import Text.ParserCombinators.Parsec

main = do
  args <- getArgs
  -- Read rawData in
  oldFile <- parseFromFile csvFile $ head args

  -- Create random number generator
  g <- newStdGen
  let ranndomGenerator = (randoms g :: [Double])
        -- Easy way to output strings instead of object:
        --Left err -> show err

      newFile = case oldFile of
        Left err -> return [show err]
        Right rawData ->
          let people = getPeople rawData
              randoms  = take (length people) ranndomGenerator
              validPairs = getValidPairs rawData people
              buckets = buildBuckets validPairs people
              matches = pickMatches buckets randoms
              partners = findPartners matches people
              listAdder (l, partner) = l ++ [partner]
              paddedData = padData rawData
          in  alphabetize $ map listAdder $ zip paddedData partners
  writeFile (args !! 1) $ genCsvFile newFile

  -- How to output string:
  --putStrLn newFile


alphabetize :: [[String]] -> [[String]]
alphabetize input =
  let compareNames left right =
        (head left) `compare` (head right)
  in  List.sortBy compareNames input

padData :: [[String]] -> [[String]]
padData rawData =
  let lengths = map length rawData
      maxLength = List.maximum lengths
      addBlanks row =
        let deficiency = maxLength - (length row)
        in  row ++ (take deficiency $ repeat "")
  in  map addBlanks rawData

getPeople :: [[String]] -> [String]
getPeople rawData =
  [person | person:partners <- rawData]

getPossiblePairs :: [[String]] -> [String] -> Set.Set (Set.Set String)
getPossiblePairs rawData people =
  let cartesianProduct = [[x, y] | x <- people, y <- people, x /= y]
  in  Set.fromList $ map Set.fromList cartesianProduct

getPastPairs :: [String] -> Set.Set (Set.Set String)
getPastPairs (person:partners) =
  Set.fromList [Set.fromList [person, partner] | partner <- partners]

getAllPastPairs :: [[String]] -> Set.Set (Set.Set String)
getAllPastPairs rawData =
  let pastPairsList = map getPastPairs rawData
  in  foldl Set.union Set.empty pastPairsList

getValidPairs :: [[String]] -> [String] -> Set.Set (Set.Set String)
getValidPairs rawData people =
  let possiblePairs = getPossiblePairs rawData people
      pastPairs = getAllPastPairs rawData
  in  Set.difference possiblePairs pastPairs

-------------------------------------------------------------------------
-------------------------------------------------------------------------

-- Algorithm for picking new pairs
-- -- Needs to remove pairs as they are picked
-- -- Needs to garauntee that no one will ever run out of pairs
--
-- Build list of buckets, one per person
-- Sort by size
-- while there are buckets
-- -- Pick a pair
-- -- Remove bucket for each person
-- -- Remove every pair either person is in

-- IDK why this typedef breaks things but it does so I leave it out.
--compareSets :: Set.Set -> Set.Set -> Ordering
compareSets a b = (Set.size a) `compare` (Set.size b)

buildBuckets :: Set.Set (Set.Set String) -> [String] -> [Set.Set (Set.Set String)]
buildBuckets validPairs people =
  let buildBucket person = Set.filter (Set.member person) validPairs
      buckets = map buildBucket people
  in  List.sortBy compareSets buckets

pruneBuckets :: Set.Set String -> [Set.Set (Set.Set String)] -> [Set.Set (Set.Set String)]
pruneBuckets match buckets =
  let doesntMatch pair = 1 > (Set.size $ Set.intersection match pair)
      prunedBuckets = map (Set.filter doesntMatch) buckets
      unsortedBuckets = filter (\x -> Set.size x > 0) prunedBuckets
  in  List.sortBy compareSets unsortedBuckets


pickMatches :: [Set.Set (Set.Set String)] -> [Double] -> [Set.Set String]
pickMatches buckets randoms =
  if [] == filter (\x -> Set.size x > 0) buckets
    then []
    else
      let headList = Set.toList $ head buckets
          listLength = fromIntegral $ length headList
          matchIndex = truncate $ listLength * (head randoms)
          match = headList !! matchIndex
      in  match:(pickMatches (pruneBuckets match buckets) $ tail randoms)

findPartners :: [Set.Set String] -> [String] -> [String]
findPartners matches people =
  map findPartner people
  where findPartner person =
          let filteredMatches = filter (Set.member person) matches
              pair =
                if [] == filteredMatches
                  then Set.fromList [person, ""]
                  else head filteredMatches
          in  head $ Set.toList $ Set.difference pair $ Set.singleton person
