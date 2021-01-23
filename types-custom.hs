-- わかり易い名前にする
-- areaOfCircle :: Double -> Double
-- areaOfCircle :: Diameter -> Area

type FirstName = String

type LastName = String

type Age = Int

type Height = Int

type PatientName = (String, String)

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

-- patientInfo :: String -> String -> Int -> Int -> String
patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height =
  name ++ " " ++ ageHeight
  where
    name = lname ++ "," ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"
