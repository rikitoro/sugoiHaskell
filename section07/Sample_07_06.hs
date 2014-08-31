module Sample_07_06 where

import qualified Data.Map as Map


type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

phoneBook =
    [ ("betty", "555-6666")
    , ("bonnie", "666-7777")
    , ("patsy", "777-8888")
    , ("lucille", "205-2928")
    , ("wendy", "939-3333")
    , ("penny", "345-1234")
    ]

---
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exit!"
    Just (state, code) -> if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
  [ (100, (Taken, "ZD391")),
    (101, (Free, "JAH3I")),
    (103, (Free, "IQSA9")),
    (105, (Free, "QQTSA")),
    (109, (Taken, "892JJ")),
    (110, (Taken, "99292"))
  ]

