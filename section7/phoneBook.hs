type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, String)]

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


