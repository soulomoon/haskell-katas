module Kyu2.InfiniteDigitalString where
import Debug.Trace

findPosition :: String -> Integer
findPosition = lun 1

lun width str = if rowBest >= 0 then rowBest else lun (width+1) str
    where rowBest = foldr (\off acc -> case geneS width off str of 
                                         (num, True) -> if (traceShow num acc) < 0 then num else min acc num
                                         _ -> acc) (-1) [0 .. width-1] 

geneS :: Int -> Int -> String -> (Integer, Bool)
geneS width offset str 
  | traceShow ("str", str, "width", width, "offset", offset, "start", start, "genS", genS, "numbers", nums) width > len = error "overflow"
  | all (=='0') str = (1 + calIndex (read $ '1':str) 0, True) 
  | otherwise = (calIndex (head nums) (toInteger offset), genS == str)
    where numlen = toInteger $ len `div` width + 1
          len = length str
          pregap = width - offset
          tailgap = width + offset - len 
          gapdiff = len - width
          frontnum = take width $ drop offset str
          tailnum = replicate (max 0 (tailgap - length pretail)) '0' ++ pretail
                   where pretail = (reverse . take tailgap . reverse) $ show $ 1 + (read $ take tailgap $ drop gapdiff str::Integer)
          start 
            | offset > 0 = prestart - 1
            | otherwise = prestart
            where prestart = read (frontnum ++ tailnum)
          nums = [start .. start + numlen]
          headnum = head nums
          genS 
            | offset == 0 = take (fromIntegral len) . concatMap show $ nums
            | otherwise = take (fromIntegral len) . drop (fromIntegral $ (toInteger . length . show) headnum - toInteger offset) . concatMap show $ nums


calIndex n offset 
    | offset == 0 = calhelper len + (n - 10 ^ len) * (len + 1)
    | otherwise = calhelper len + (n - 10 ^ len) * (len + 1) + width - offset
        where len = toInteger $ length (show n) - 1 
              width = len + 1

calhelper :: Integer -> Integer
calhelper 0 = 0
calhelper 1 = 9 
calhelper k = (10 ^ k - 10 ^ (k-1)) * k + calhelper (k - 1)
