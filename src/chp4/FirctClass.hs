ifEven :: Integral p => p -> (p -> p) -> p
ifEven n func =
  if even n
    then func n
    else n

inc :: Num a => a -> a
inc n = n + 1

double :: Num a => a -> a
double n = n * 2

square :: Num a => a -> a
square n = n ^ (2 :: Int)

ifEvenInc :: Integral p => p -> p
ifEvenInc n = ifEven n inc

ifEvenDouble :: Integral p => p -> p
ifEvenDouble n = ifEven n double

ifEvenSquare :: Integral p => p -> p
ifEvenSquare n = ifEven n square

names :: [([Char], [Char])]
names =
  [ ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephen", "Morris"),
    ("Test", "Sumner")
  ]

--compareNames :: Ord a => (p -> a) -> p -> p -> Ordering
--compareNames func name1 name2=
--  let funcName1=func name1
--      funcName2=func name2
--  in
--    if funcName1>funcName2
--    then GT
--    else if funcName1<funcName2
--      then LT
--      else EQ

compareNames :: Ord a => (p -> a) -> p -> p -> Ordering
compareNames func name1 name2 =
  let funcName1 = func name1
      funcName2 = func name2
   in compare funcName1 funcName2

compareLastFamilyNames :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
compareLastFamilyNames name1 name2 =
  let resultCompareLastName = compareNames snd name1 name2
      resultCompareFamilyName = compareNames fst name1 name2
   in case resultCompareLastName of
        EQ -> resultCompareFamilyName
        _ -> resultCompareLastName

sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name =
  let lastName = snd name
      nameText = fst name ++ " " ++ lastName
   in if lastName < "L"
        then nameText ++ " - PO Box 1234 - San Fransisco , CA , 94111"
        else nameText ++ " - PO Box 1010 - San Fransisco , CA , 94109"

nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name =
  let nameText = fst name ++ " " ++ snd name
   in nameText ++ ": PO Box 789 - New York , NY , 10013"

renoOffice :: (a, [Char]) -> [Char]
renoOffice name =
  let nameText = snd name
   in nameText ++ " - PO Box 456 - Reno, NV 89523"

dcOffice :: ([Char], [Char]) -> [Char]
dcOffice name = fst name ++ " " ++ snd name ++ " Esq"

getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location =
  case location of
    "sf" -> sfOffice
    "ny" -> nyOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> fst name ++ " " ++ snd name)

addressLetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetter name location =
  let getLocation = getLocationFunction location
   in getLocation name
