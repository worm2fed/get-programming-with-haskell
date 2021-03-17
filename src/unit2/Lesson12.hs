patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
    where name = lname ++ ", " ++ fname
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type FirstName = String
type LastName = String
type Age = Int
type Height = Int

-- patientInfo :: FirstName -> LastName -> Age -> Height -> String

type PatientName = (FirstName, LastName)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' name age height = prepareName ++ " " ++ prepareAgeHeight
    where prepareName = firstName name ++ ", " ++ lastName name
          prepareAgeHeight = "(" ++ show age ++ "yrs, " ++ show height ++ "in)"


data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
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
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _               = True
canDonateTo _ (BloodType AB _)              = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _                             = False

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l)             = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"


data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 25 70 150 (BloodType O Pos)

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt

data Patient' = Patient' { name      :: Name
                         , sex       ::  Sex
                         , age       :: Int
                         , height    :: Int
                         , weight    :: Int
                         , bloodType :: BloodType
                         }

jackieSmith :: Patient'
jackieSmith = Patient' { name = Name "Jackie" "Smith"
                       , age = 43
                       , sex = Female
                       , height = 62
                       , weight = 115
                       , bloodType = BloodType O Neg
                       }

-- showName (name jackieSmith)

jackieSmithUpdated = jackieSmith { age = 44 }


-- Q12.1 Write a function similar to canDonateTo that takes
-- two patients as arguments rather than two BloodTypes.
canDonateTo' :: Patient' -> Patient' -> Bool
canDonateTo' a b               = canDonateTo (bloodType a) (bloodType b)


-- Q12.2 Implement a patientSummary function that uses your
-- final Patient type. patient- Summary should output a
-- string that looks like this:
--  **************
--  Patient Name: Smith, John
--  Sex: Male
--  Age: 46
--  Height: 72 in.
--  Weight: 210 lbs.
--  Blood Type: AB+
--  **************
-- If you need to, feel free to create useful helper functions.
patientSummary :: Patient -> String
patientSummary (Patient name sex age height weight blood) =
        "\nPatient name: " ++ showName name  ++
        "\nSex: " ++ showSex  sex ++
        "\nAge: " ++ show age ++
        "\nHeight: " ++ show height ++
        "\nWeight: " ++ show weight ++
        "\nBlood Type: " ++ showBloodType blood
    where showSex Male   = "Male"
          showSex Female = "Female"