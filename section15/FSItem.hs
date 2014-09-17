module FSItem where

import Data.List (break)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show, Eq)

myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "goat.wav" "baaaaaaaaaaaaaaaaaaaaaa"
    , File "Pope.avi" "god bless"
    , Folder "pics"
      [ File "ape.jpg" "bleargh"
      , File "watermelon.gif" "smash"
      , File "skull.bpm" "Yikes"
      ]
    , File "dijon.doc" "best mustard"
    , Folder "programs"
      [ File "far.exe" "10gotofart"
      , File "owl.dmg" "mov eax h00t"
      , File "not_a_virus.exe" "really not a virus"
      , Folder "source_code" 
        [ File "best.hs" "main = print (fix error)"
        , File "random.hs" "ain = print 4"
        ]
      ]
    ]



data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) =
    (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs : bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

(-:) :: a -> (a -> b) -> b
f -: g = g f

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)