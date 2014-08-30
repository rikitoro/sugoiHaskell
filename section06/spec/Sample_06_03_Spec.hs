module Sample_06_03_Spec where

import Test.Hspec 
import Test.QuickCheck
import Control.Exception (evaluate)
import Sample_06_03
import qualified Data.Map as Map

main = hspec $ do
  describe "findKey'" $ do
    it "キーでリストを検索し値を返す" $ do
      findKey' "lucille" phoneBook `shouldBe` "205-2928"
    it "リストに探しているキーがなかった場合はエラーを投げる" $ do
      evaluate (findKey' "hoge" phoneBook) `shouldThrow` anyErrorCall
  describe "findKey" $ do
    it "キーでリストを検索しJustでくるんだ値を返す" $ do
      findKey "lucille" phoneBook `shouldBe` Just "205-2928"
    it "リストに探しているキーがなかった場合はNothingを返す" $ do
      findKey "hoge" phoneBook `shouldBe` Nothing
  describe "findKey''" $ do 
    it "キーでリストを検索しJustでくるんだ値を返す" $ do
      findKey'' "lucille" phoneBook `shouldBe` Just "205-2928"
    it "リストに探しているキーがなかった場合はNothingを返す" $ do
      findKey'' "hoge" phoneBook `shouldBe` Nothing
    it "findKeyをfoldrで定義" $ do
      property $ \key xs -> findKey'' (key :: String) (xs :: [(String, String)]) == findKey key xs

  describe "Map.fromList" $ do
    it "生成されるマップは渡される連想リスト内の順序には関係しない" $ do
      Map.fromList [("MS",1),("AP",2)] `shouldBe` Map.fromList [("AP",2),("MS",1)]
    it "連想リストに重複したキーがある場合はあとの方の要素が使われる" $ do
      Map.fromList [("MS",1),("AP",2),("MS",3)] `shouldBe` Map.fromList [("MS",3),("AP",2)]
  describe "Map.lookup" $ do
    it "キーとマップを渡すと対応する値を検索し成功したらJustでくるんで返す" $ do
      Map.lookup "lucille" phoneBookMap `shouldBe` Just "205-2928"
    it "検索に失敗するとNothingを返す" $ do
      Map.lookup "hoge" phoneBookMap `shouldBe` Nothing
  describe "Map.insert" $ do
    it "Mapにキーと値を挿入して新しいMapを返す" $ do
      let newPhoneBookMap = Map.insert "grace" "341-9021" phoneBookMap
      Map.lookup "grace" newPhoneBookMap `shouldBe` Just "341-9021"
  describe "Map.size" $ do
    it "Mapを渡すとそのサイズを返す" $ do
      Map.size phoneBookMap `shouldBe` 6

  describe "string2digits" $ do 
    it "文字列の電話番号を数値のリストに変換する" $ do
      string2digits "022-345-0987" `shouldBe` [0,2,2,3,4,5,0,9,8,7] 

  describe "Map.map" $ do
    it "phoneBookMapをstring2digitでマップする" $ do
      let intBookMap = Map.map string2digits phoneBookMap
      Map.lookup "lucille" intBookMap `shouldBe` Just [2,0,5,2,9,2,8]

  describe "phoeBookToMap" $ do
    it "連想リストに重複したキーがあるときは値の文字列をカンマで結合させてMapを作る" $ do
      (Map.lookup "patsy" $ phoneBookToMap phoneBook'' ) `shouldBe` Just "111-1111, 493-2928"

  describe "phoneBookToMap'" $ do
    it "連想リストの各キーに対する値をリストにしてMapに変換する" $ do
      (Map.lookup "bonnie" $ phoneBookToMap' phoneBook'') `shouldBe` Just ["000-0000","452-2928"]

  describe "Map.fromListWith" $ do
    let list = [(2,3),(2,5),(2,100),(3,22),(3,29),(3,11),(4,22),(4,15)]
    it "番号の連想リストから各キーに対する値の最大値を保持するMapを作る" $ do
      Map.fromListWith max list `shouldBe` Map.fromList [(2,100), (3,29), (4,22)]
    it "同じキーの値を足し合わせてMapを作る" $ do
      Map.fromListWith (+) list `shouldBe` Map.fromList [(2,108), (3,62), (4,37)]