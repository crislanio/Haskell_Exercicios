import qualified Data.Map as Map

data Func = Func { nome      :: String
                 , numVendas :: Int
                 , total     :: Float
                 } deriving (Show)
type ID = String          
type Report = Map.Map ID Func 

-----------------

newFunc n = Func n 0 0

loadReport' :: [[String]] -> Report
loadReport' l = Map.fromList (map (\(x:y) -> (x, newFunc (unwords y) )) l)

loadReport c = loadReport' (map words (lines c))

-----------------

reportToStr' [] ll = reverse ll
reportToStr' (x:l) ll = reportToStr' l (( show (fst x) ++ " - " ++ show (snd x) ):ll)

reportToStr r = unlines (reportToStr' (Map.toList r) [])

-----------------

loadLote' l = (map (\(x:y:[]) -> ( (read x :: Float), (read y :: Float) )) l)

loadLote c = loadLote' (map words (lines c))



add (x, y) (i, Func a b c) = (i, Func a (b+1) (c+y))

coisa x [] = []
coisa x (y:rep) = if (fst x) == (fst y) then (add x y):rep 
                                        else y:(coisa x rep)


--porcLote' 

procLote (x:lot) rep = Map.fromList (coisa x (Map.toList rep)) 

------------------

main = do
	contents <- readFile "funcionarios.dat"
	let r = (loadReport contents)
	lote <- readFile "2013/JAN/lote01.dat"
	let l = (loadLote lote)
	writeFile "report.dat" (reportToStr r)

