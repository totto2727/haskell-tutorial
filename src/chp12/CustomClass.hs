type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name
      { firstName :: FirstName,
        lastName :: LastName
      }
  | NameWithMiddle
      { firstName :: FirstName,
        middleName :: MiddleName,
        lastName :: LastName
      }

type Age = Int

type Height = Int

type Weight = Int

--data Patient = Patient Name Sex Age Height Weight BloodType
data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Age,
    height :: Height,
    weight :: Weight,
    bloodType :: BloodType
  }

data Sex = Male | Female

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

patient :: Patient
patient = Patient (NameWithMiddle "a" "b" "c") Male 19 170 60 (BloodType A Pos)

patient1 :: Patient
patient1 =
  Patient
    { age = 21,
      sex = Female,
      weight = 50,
      height = 155,
      bloodType = BloodType O Neg,
      name = Name "a" "b"
    }

patientInfo :: Patient -> String
patientInfo (Patient n s a h w b) = patientName ++ " " ++ sahwb
  where
    patientName = showName n
    sexAge = showSexInitial s ++ " " ++ show a ++ "yrs. "
    heightWeightBloodType = show h ++ "in. " ++ show w ++ "lb. " ++ showBloodType b
    sahwb = "(" ++ sexAge ++ heightWeightBloodType ++ ")"

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

showSexInitial :: Sex -> String
showSexInitial Male = "M"
showSexInitial Female = "F"

showSex::Sex->String
showSex Male="Male"
showSex Female="FeMale"

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO O = "O"
showABO AB = "AB"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) _ = True
canDonateTo (BloodType B _) _ = True
canDonateTo _ _ = False

donorFor :: Patient -> Patient -> Bool
donorFor donor recipient = canDonateTo donorBloodType recipientBloodType
  where
    donorBloodType = bloodType donor
    recipientBloodType = bloodType recipient

patientSummary::Patient->String
patientSummary p=
  "**************"++
  "\nPatient Name: "++showName (name p)++
  "\nSex: "++showSex (sex p)++
  "\nAge: "++show (age p)++
  "\nHeight: "++show (height p)++
  "\nWeight: "++show (weight p)++
  "\nBloosType"++showBloodType (bloodType p)++
  "\n**************\n"
  
main :: IO ()
main=do
  print (patientSummary patient)
  