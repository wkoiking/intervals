module Interval where

-- base
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.Monoid
import MyParser.Numeral
import MyParser.String
import MyParser.Text
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Control.Monad

type Interval a = ((Double, Double), a)

-- sample
data MyInt = My Int deriving (Show, Eq)
instance Monoid MyInt where
 My i `mappend` My j = My (min i j)
 mempty = My (maxBound :: Int)

intervalP :: Parser (Interval MyInt)
intervalP = do
        d1 <- liftM fromIntegral decimal
        d2 <- liftM fromIntegral decimal
        i3 <- liftM fromIntegral integer
        guard $ d1 < d2
        return ((d1,d2), My i3)

showInterval :: Interval MyInt -> String
showInterval ((d1,d2), My i3) = show d1 ++ "\t" ++ show d2 ++ "\t" ++ show i3

-- algorithm
joinIntervals :: (Eq a, Monoid a) => [Interval a] -> [Interval a]
joinIntervals = sanitize . foldr joinInterval [] . sortBy (comparing fst)

joinInterval :: Monoid a => (Interval a) -> [Interval a] -> [Interval a]
joinInterval x [] = [x]
joinInterval x (y:ys) = init intvls ++ joinInterval (last intvls) ys
 where intvls = joinTwoIntervals x y

joinTwoIntervals :: Monoid a => Interval a -> Interval a -> [Interval a]
joinTwoIntervals intvl@((s,e), lim) intvl'@((s',e'), lim')
    | e < s'    = [intvl, intvl']
    | e' < e    = filter (uncurry (<) . fst) [((s,s'), lim), ((s',e'), lim2), ((e',e), lim)]
    | otherwise = filter (uncurry (<) . fst) [((s,s'), lim), ((s',e), lim2), ((e,e'), lim')]
 where lim2 = lim <> lim'

sanitize :: (Eq a) => [Interval a] -> [Interval a]
sanitize [] = []
sanitize [x] = [x]
sanitize (intvl@((s,e), lim) : intvl'@((s',e'), lim') : rest)
    | lim == lim' = sanitize (((s, e'), lim) : rest)
    | otherwise   = intvl : sanitize (intvl' : rest)

-- extra utility

reverseIntervals :: [Interval a] -> [Interval a]
reverseIntervals = reverse . map (\ (x,y) -> (swap x, y))

fillInterval :: (Eq a, Monoid a) => Double -> Double -> [Interval a] -> [Interval a]
fillInterval s e intvls = joinIntervals $ ((s,e), mempty):intvls
