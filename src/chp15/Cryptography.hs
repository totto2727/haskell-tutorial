data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Bounded, Enum)

rotNEncoder :: Enum a => Int -> a -> a
rotNEncoder alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

rotNDecoder :: Enum a => Int -> a -> a
rotNDecoder alphabetSize c = toEnum rotation
  where
    halfAlphabetSize = alphabetSize `div` 2
    offset = halfAlphabetSize + fromEnum c
    evenOrOdd =
      if even alphabetSize
        then offset
        else 1 + offset
    rotation = evenOrOdd `mod` alphabetSize

rotStringEncoder :: String -> String
rotStringEncoder = map rotNCharEncoder
  where
    alphabetSize = 1 + fromEnum (maxBound :: Char)
    rotNCharEncoder = rotNEncoder alphabetSize

rotStringDecoder :: String -> String
rotStringDecoder = map rotNCharDecoder
  where
    alphabetSize = 1 + fromEnum (maxBound :: Char)
    rotNCharDecoder = rotNDecoder alphabetSize

rotChar :: Char -> Char
rotChar = rotNEncoder sizeOfAlphabet
  where
    sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

{-
threeLetterEncoder::[ThreeLetterAlphabet]->[ThreeLetterAlphabet]
threeLetterEncoder=map rot3l
  where
    alphaSize=1+fromEnum (maxBound::ThreeLetterAlphabet)
    rot3l=rotN alphaSize

threeLetterDecoder::[ThreeLetterAlphabet]->[ThreeLetterAlphabet]
threeLetterDecoder=map rot3lDecoder
  where
    alphaSize=1+fromEnum (maxBound::ThreeLetterAlphabet)
    rot3lDecoder=rotNDecoder alphaSize-}

xorBool :: Bool -> Bool -> Bool
xorBool bool1 bool2 = (bool1 || bool2) && not (bool1 && bool2)

xorBoolPair :: (Bool, Bool) -> Bool
xorBoolPair (b1, b2) = xorBool b1 b2

{-
xor::[Bool]->[Bool]->[Bool]
xor list1 list2=map xorBoolPair (zip list1 list2)
-}

xor :: [Bool] -> [Bool] -> [Bool]
xor = zipWith xorBool

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if even n
    then False : intToBits' halfN
    else True : intToBits' halfN
  where
    halfN = n `div` 2

maxBits :: Int
maxBits = length $ intToBits' (maxBound :: Int)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reverseBits
  where
    reverseBits = reverse $ intToBits' n
    missingBits = maxBits - length reverseBits
    leadingFalses = replicate missingBits False

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ snd x) trueLocations)
  where
    size = length bits
    indexes = [size -1, size -2 .. 0]
    trueLocations = filter fst (zip bits indexes)

charToBits :: Char -> Bits
charToBits c = intToBits $ fromEnum c

bitsToChar :: Bits -> Char
bitsToChar b = toEnum $ bitsToInt b

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = zipWith xor padBits plainTextBits
  where
    padBits = map charToBits pad
    plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar $ applyOTP' pad plainText

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot = rotStringEncoder
  decode Rot = rotStringDecoder

newtype OneTimeTable = OTP String

instance Cipher OneTimeTable where
  encode (OTP pad) = applyOTP pad
  decode (OTP pad) = applyOTP pad

myPad::OneTimeTable
myPad=OTP $ cycle [minBound .. maxBound]