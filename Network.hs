module Network where

import Numeric.LinearAlgebra
import System.Random
import qualified Data.Vector.Storable as V

type FFN = [Matrix Double]
type DV  = Vector Double
--          Adjacenci    ,State        ,Tresholds    ,Output ,Reduction    ,Active Reduction ,InputOutput neuron number
type MSN = (Matrix Double,Vector Double,Vector Double,(DV,DV),Matrix Double,Matrix Double,Int,Int)

createNetworkFF :: [Int] -> FFN
createNetworkFF [] = []
createNetworkFF [x] = []
createNetworkFF (x:y:xs) = (this : next)
   where this = (x><y) (randoms std)
         std = mkStdGen 0
         next = createNetworkFF (y:xs)

runNetworkFF :: Vector Double -> FFN -> Vector Double
runNetworkFF input [x]      = V.map tanh (input <> x)
runNetworkFF input (x:xs)   = runNetworkFF nextIn xs
   where nextIn = V.map tanh (input <> x)

-- Number of Neurons
-- Number of Inputs
-- Number of OutPuts
createNetworkMS :: (RandomGen g) => Int -> Int -> Int -> g -> MSN
createNetworkMS n i e std = (adj,sta,thr,(o1,o2),red,ard,i,n-e)
   where rlist = randoms std
         (adj,rlist1) = withN (n><n) (n^2) rlist
         (sta,rlist2) = withN fromList n   rlist1
         (thr,rlist3) = withN fromList n   rlist2
         (o1 ,rlist4) = withN fromList n   rlist3
         (o2 ,rlist5) = withN fromList n   rlist4
         (red,rlist6) = withN (n><2) (n*2) rlist5
         (ard,rlist7) = withN (n><2) (n*2) rlist6

--Mutate a network with given mutation factor
mutateNetworkMS :: (RandomGen g) => MSN -> Double-> g -> MSN
mutateNetworkMS (adj,sta,thr,(o1,o2),red,ard,i,e) m std = (nadj,nsta,nthr,(no1,no2),nred,nard,i,e)
   where rlist = randomRs (-m,m) std
         (nadj,rlist1) = withN (n><n) (n^2) rlist
         (nsta,rlist2) = withN fromList n   rlist1
         (nthr,rlist3) = withN fromList n   rlist2
         (no1 ,rlist4) = withN fromList n   rlist3
         (no2 ,rlist5) = withN fromList n   rlist4
         (nred,rlist6) = withN (n><2) (n*2) rlist5
         (nard,rlist7) = withN (n><2) (n*2) rlist6

withN :: ([a] -> b) -> Int -> [a] -> (b,[a])
withN f n l = (f h,t)
   where (h,t) = splitAt n l

runNetworkMSIO :: Vector Double -> MSN -> IO (MSN)
runNetworkMSIO a b = do
   let (out,n) = runNetworkMS a b
   print out
   return n

runNetworkMS :: Vector Double -> MSN -> (Vector Bool,MSN)
runNetworkMS input (adj,sta,thr,(o1,o2),red,ard,i,e) = (erg,(adj,nsta3,thr,(o1,o2),red,ard,i,e))
   where --Set the state of the input neurons
         nsta = input V.++ (V.drop i sta)
         --Get the output of all Neurons (has linear and static compont)
         out = V.zipWith (+) (V.zipWith (*) nsta o1) o2
         --Figur out which Neuons are active
         act = V.zipWith (>) thr nsta
         --Set the output of all inactive neurons to 0
         outf = V.zipWith mfilter act out
         --Calculate the input for all Neurons
         inp = out <> adj
         --Add the inputs to the State
         nsta2 = V.zipWith (+) inp nsta
         --Caclulate the reductions
         ared = V.zipWith3 select act red ard
         --Subtract the reductions
         nsta3 = V.zipWith (-) nsta2 ared
         --Get the activation of the output vec
         erg = V.drop e act
         mfilter :: Bool -> Double -> Double
         mfilter True a = a
         mfilter False _ = 0
         select :: Bool -> Double -> Double -> Double
         select True _ r = r
         select False r _ = r

