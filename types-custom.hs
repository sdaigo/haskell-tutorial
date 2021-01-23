-- わかり易い名前にする
-- areaOfCircle :: Double -> Double
-- areaOfCircle :: Diameter -> Area

type FirstName = String

type LastName = String

type MiddleName = String

type Age = Int

type Height = Int

type PatientName = (String, String)

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"

name2 = NameWithMiddle "Jerome" "David" "Salinger"

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

-- 血液型
data RhType = Pos | Neg

data ABOType = A | B | AB | O

-- Type constructor = Data constructor ...
-- BloodTypeはRhTypeを持つABOTypeである
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

{-
* AはAとABのドナーになれる
* BはBとABのドナーになれる
* ABはABのドナーにしかなれない
* OはA、AB、Oのドナーになれる。
-}
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True -- Oからはどの血液型でもOK
canDonateTo _ (BloodType AB _) = True -- どの血液型もABへはOK
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

donorFor :: Patient -> Patient -> Bool
donorFor p1 p2 =
  canDonateTo (bloodType p1) (bloodType p2)

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeElizabethSmith :: Patient
janeElizabethSmith =
  Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 30 50 150 (BloodType O Neg)

jackieSmith :: Patient
jackieSmith =
  Patient
    { name = Name "Jackie" "Smith",
      age = 43,
      sex = Female,
      height = 62,
      weight = 115,
      bloodType = BloodType O Neg
    }

showSex Male = "Male"
showSex Female = "Female"

patientSummary :: Patient -> String
patientSummary p =
  "*************\n"
    ++ "Patinet Name: "
    ++ showName (name p)
    ++ "\n"
    ++ "Sex: "
    ++ showSex (sex p)
    ++ "\n"
    ++ "Age: "
    ++ show (age p)
    ++ "\n"
    ++ "Height: "
    ++ show (height p)
    ++ "\n"
    ++ "Weight: "
    ++ show (weight p)
    ++ "\n"
    ++ "Blood Type: "
    ++ showBloodType (bloodType p)
    ++ "\n*************\n"

-- patientInfo :: String -> String -> Int -> Int -> String
patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height =
  name ++ " " ++ ageHeight
  where
    name = lname ++ "," ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"
