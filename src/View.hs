module View where


grid :: Int -> Int -> [[Char]]
grid x y = let row = ['_' | _<-[0..x]] 
           in [row | _<-[0..y]]

ranges :: [(Int,Int)] -> (Int,Int)
ranges grid = let firsts  = map (\(f,s) -> f) grid
                  seconds = map (\(fs,sn) -> sn) grid
              in 
                  (maximum firsts,maximum seconds) 

showGrid :: [(Int,Int)] -> [IO ()]
showGrid grd = let xy = ranges grd 
                   emptyGrid = foldl (\acc row -> (putStrLn row):acc ) [] (grid (fst xy) (snd xy)) 
               in
               [putStrLn "2"]

insertCells :: [(Int,Int)]->[[Char]]->[[Char]]
insertCells cells grid = foldr (\cell newGrd -> 

maiin = do result <- sequence $ showGrid [(1,1),(3,3)] 
           print "Finished" 
