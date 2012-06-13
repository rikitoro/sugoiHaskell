data CoolBool' = CoolBool' { getCoolBool' :: Bool } deriving (Show)

helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello" 

newtype CoolBool = CoolBool { getCoolBool :: Bool } deriving (Show)

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"