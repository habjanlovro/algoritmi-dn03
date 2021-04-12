import Data.List


type Point = (Double, Double)


-- Naloga A - unija dveh konveksnih ovojnic
convexHullUnion :: [Point] -> [Point] -> [Point]
convexHullUnion s1 s2 =
  let points = nubBy (\p1 p2 -> p1 == p2) (s1 ++ s2)
      startPoint = minimumBy (\(_, y1) (_, y2) -> compare y1 y2) points
      noStart = filter (\p -> p /= startPoint) points
      sortedPoints = sortBy (\p1 p2 -> compare 0 (ccw startPoint p1 p2)) noStart
  in
    reverse $ convexHullCall (drop 2 sortedPoints) (reverse (startPoint : (take 2 sortedPoints)))

convexHullCall :: [Point] -> [Point] -> [Point]
convexHullCall [] stack = stack
convexHullCall (p : ps) stack =
  convexHullCall ps (fixStack p stack)

fixStack :: Point -> [Point] -> [Point]
fixStack point stack =
  if length stack > 1 then
    let p1 = stack !! 0
        p0 = stack !! 1
        direction = ccw p0 p1 point
    in
      if direction <= 0 then
        fixStack point (tail stack)
      else
        point : stack
  else
    point : stack

ccw :: Point -> Point -> Point -> Double
ccw (x0, y0) (x1, y1) (x2, y2) =
  (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)


-- Naloga B - ali se dve konveksni ovojnici sekata
doesIntersect :: [Point] -> [Point] -> Bool
doesIntersect s1 s2 =
  if any (\p -> isElement s2 p ) s1 then True
  else doesIntersectCall s1 (getPlaneSegments s2)

getPlaneSegments :: [Point] -> [(Point, Point)]
getPlaneSegments convexHull =
  let first = head convexHull
      call [] segs = segs
      call [p] segs = (p, first) : segs
      call (p1 : p2 : ps) segs = call (p2 : ps) ((p1, p2) : segs)
  in
    call convexHull []

doesIntersectCall :: [Point] -> [(Point, Point)] -> Bool
doesIntersectCall [] _ = False
doesIntersectCall (p : ps) planeSegments =
  let numInter = numIntersectionsPoint p planeSegments
  in
    if numInter `mod` 2 == 1 then True
    else doesIntersectCall ps planeSegments

numIntersectionsPoint :: Point -> [(Point, Point)] -> Int
numIntersectionsPoint p segs =
  let call [] n = n
      call (s : sgs) n =
        if pointIntersectsSegment p s
        then call sgs (n + 1) else call sgs n
  in
    call segs 0

pointIntersectsSegment :: Point -> (Point, Point) -> Bool
pointIntersectsSegment p (ps, pe) =
  let direction = (1.0, 0.0)
      line = vecSub pe ps
  in
    if snd p == snd ps && fst p <= fst ps then False
    else
      case compare 0.0 (crossProduct direction line) of
        EQ ->
          let minX = min (fst ps) (fst pe)
              minY = min (snd ps) (snd pe)
              maxX = max (fst ps) (fst pe)
              maxY = max (snd ps) (snd pe)
          in
            minX < fst p && minY < snd p && maxX > fst p && maxY > snd p

        _ ->
          let tRay = crossProduct (vecSub ps p) line / crossProduct direction line
              tSegment = crossProduct (vecSub ps p) direction / crossProduct direction line
          in
            if tRay >= 0 && tSegment >= 0 && tSegment <= 1 then True else False

isElement :: Eq a => [a] -> a -> Bool
isElement [] _ = False
isElement (x : xs) p =
  if x == p then True
  else isElement xs p

crossProduct :: Point -> Point -> Double
crossProduct (x1, y1) (x2, y2) =
  x1 * y2 - y1 * x2

vecSub :: Point -> Point -> Point
vecSub (x1, y1) (x2, y2) =
  (x1 - x2, y1 - y2)


main :: IO()
main = do
  s1 <- getData
  s2 <- getData
  printUnion $ convexHullUnion s1 s2
  printIntersect $ doesIntersect s1 s2

getData :: IO ([(Double, Double)])
getData = do
  xs <- getLine
  ys <- getLine
  let parsedXs = map read (words xs) :: [Double]
  let parsedYs = map read (words ys) :: [Double]
  let hull = zip parsedXs parsedYs
  return hull

printUnion :: [(Double, Double)] -> IO ()
printUnion s =
  let (xs, ys) = unzip s

      printList [] = putStr "\n"
      printList (x : xes) = do
        putStr $ show x ++ " "
        printList xes
  in do
    printList xs
    printList ys

printIntersect :: Bool -> IO ()
printIntersect True = putStrLn "TRUE"
printIntersect False = putStrLn "FALSE"
