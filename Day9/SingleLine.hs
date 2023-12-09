main = readFile "input.txt" >>= print . sum . concatMap (map last . takeWhile (not . all (==0)) . iterate (zipWith (-) <$> tail <*> init) . map read . words) . lines
