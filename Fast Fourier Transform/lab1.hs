import Data.Complex

data Poly a = Poly [a] deriving (Show)

polyCoeffs :: Poly a -> [a]
polyCoeffs (Poly [a]) =[a]

polyEval :: Num a => [a] -> a -> a
polyEval poly x = sum $ (zipWith (*) poly (iterate (*x) 1))

polyAdd :: Num a => [a] -> [a] -> [a]
polyAdd p1 p2 = zipWith (+) p1 p2

polyScalMult :: Num a => [a] -> a -> [a]
polyScalMult p1 a = map (*a) p1

polyNegate :: Num a => [a] -> [a]
polyNegate p1 = polyScalMult p1 (-1)

polyDiff :: Num a => [a] -> [a] -> [a]
polyDiff p1 p2 = polyAdd p1 (polyNegate p2)

polyAbs :: Num a => [a] -> [a]
polyAbs p1 = map (abs) p1

polyApproxEqual :: [Integer] -> [Integer] -> Bool
error_tol = 1
coeffDiff p1 p2 = sum ( polyAbs ( zipWith (-) p1 p2))
polyApproxEqual p1 p2 = ((coeffDiff p1 p2) < error_tol)

polyMult :: Num a => [a] -> [a] -> [a]
polyMult a b = [coeffk a b k | k <- [1 .. ((length a)*(length b))] ] where
	coeffk a b k = sum (zipWith (*) (take (k+1) a) (reverse (take (k+1) b)) )	



data PolyPoints n x y = PolyPoints n [x] [y] deriving (Show)

toPolyPoints :: Num y => [y] -> [y] -> PolyPoints Int y y
toPolyPoints poly x
	| (length x  >= length poly) = PolyPoints (length poly) x (evalLs poly x)
	| otherwise = undefined
	where
		evalLs poly [] = [] 
		evalLs poly (c:cs) = (polyEval poly c) : evalLs poly cs

polyPointsAdd :: Num a => [a] -> [a] -> [a]
polyPointsAdd p1 p2 = zipWith (+) p1 p2

polyPointsScalMult :: Num a => [a] -> a -> [a]
polyPointsScalMult p1 a = map (*a) p1

polyPointsMult :: Num a => [a] -> [a] -> [a]
polyPointsMult p1 p2 = zipWith (*) p1 p2

getEven :: [a] -> [a]
getEven [] = []
getEven (c:d:cs) = c : getEven (cs)

getOdd :: [a] -> [a]
getOdd [] = []
getOdd (c:d:cs) = d : getOdd (cs)

getRoots :: RealFloat a => Complex a -> Complex a -> [Complex a]
getRoots n 0 = []
getRoots n j = exp (((-2)*pi*(0:+1)*j)/n) : getRoots n (j-1)

evalEven :: Num a => [a] -> [a] -> [a]
evalEven poly [] = []
evalEven poly (w:ws) = (polyEval (getEven poly) w ) : evalEven poly ws

evalOdd :: Num a => [a] -> [a] -> [a]
evalOdd poly [] = []
evalOdd poly (w:ws) = (polyEval (getOdd poly) w ) : evalOdd poly ws

fft_even :: RealFloat a => [Complex a] -> Complex a -> [Complex a]
fft_even poly n = evalEven poly (getRoots (n/2) (n/2))

fft_odd :: RealFloat a => [Complex a] -> Complex a -> [Complex a]
fft_odd poly n = evalOdd poly (getRoots (n/2) (n/2))



data PolyPointsFFT a = PolyPointsFFT [a] deriving (Show)

fft :: RealFloat a => [Complex a] -> [Complex a]
fft poly = zipWith (+) ((fft_even poly n) ++ (fft_even poly n)) (zipWith (*) (getRoots n n) ((fft_odd poly n) ++ (fft_odd poly n))) 
	where n = fromIntegral $ length poly

invfft ::  [a] -> [Complex Double]
invfft poly = zipWithInvN (invfft' poly) (fromIntegral $ length poly) 
	where zipWithInvN ls n = zipWith (*) (replicate n (fromRational(1/(fromIntegral n)))) ls
	      invfft' ls = conjLS $ fft $ conjLS $ getRoots n n
	      n = fromIntegral $ length poly
	      conjLS [] = []
	      conjLS (c:cs) = conjugate c : conjLS cs

fromPolyPointsFFT :: [a] -> PolyPointsFFT (Complex Double)
fromPolyPointsFFT poly = PolyPointsFFT $ invfft poly

polyMultFFT :: RealFloat a => [Complex a] -> [Complex a] -> [Complex Double]	
polyMultFFT p1 p2 = invfft $ zipWith (*) (fft p1) (fft p2) 

