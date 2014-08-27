module Sample_05_03_Spec where

import Test.Hspec

import Sample_05_03

main :: IO ()
main = hspec $ do
  describe "map" $ do
    it "(+3)と数リストを渡すとリストの各要素に3を加えたリストを返す" $ do
      map (+3) [1,5,3,1,6] `shouldBe` [4,8,6,4,9]
    it "(++\"!\")と文字列からなるリストを渡すと各文字列に\"!\"を追加する" $ do
      map (++"!") ["BIFF", "BANG", "POW"] `shouldBe` ["BIFF!", "BANG!", "POW!"]
    it "(replicate 3)とリストを渡すとリストの各要素を3回繰り返す" $ do
      map (replicate 3) [1..3] `shouldBe` [[1,1,1],[2,2,2],[3,3,3]]
    it "ネストしたリストに対してmapを適応する" $ do
      map (map (^2)) [[1,2],[3,4,5],[6]] `shouldBe` [[1,4],[9,16,25],[36]]
    it "fstとタプルからなるリストを渡すと各タプルの先頭要素からなるリストを返す" $ do
      map fst [(1,2),(3,4),(5,6)] `shouldBe` [1,3,5]
    it "2引数関数とリストを渡すと1引数関数のリストを返す" $ do
      let listOfFuns = map (*) [0..]
      (listOfFuns !! 4) 5 `shouldBe` 20

  describe "filter" $ do
    it "(>3)と数リストを渡すと3より大きな要素を取り出したリストを返す" $ do
      filter (>3) [1,5,2,6,3] `shouldBe` [5,6]
    it "(==3)と数リストを渡すとリストから3だけを取り出す" $ do
      filter (==3) [1,3,5,3,2] `shouldBe` [3,3]
    it "evenと数リストを渡すと偶数要素を取り出す" $ do
      filter even [1..10] `shouldBe` [2,4,6,8,10]
    it "nullでない要素だけを取り出す" $ do
      let notNull x = not (null x)
      filter notNull [[1,2,3],[],[4,5],[],[],[6]] `shouldBe` [[1,2,3],[4,5],[6]]
    it "文字列から小文字だけを取り出す" $ do
      filter (`elem` ['a'..'z']) "aG  bHI JKcL  MdeN fOP" `shouldBe` "abcdef"
    it "文字列から大文字だけを取り出す" $ do
      filter (`elem` ['A'..'Z']) "aG  bHI JKcL  MdeN fOP" `shouldBe` "GHIJKLMNOP"
    it "等価なリスト内包表記と比較" $ do
      filter (<15) (filter even [1..20]) `shouldBe` [x| x <- [1..20], x < 15, even x]

  describe "quicksort" $ do
    it "数リストを渡すとソートして返す" $ do
      quicksort [9,5,3,7,5,3,1] `shouldBe` [1,3,3,5,5,7,9]

  describe "largestDivisible" $ do
    it "100000以下の数で3829で割り切れる最大の数99554を返す" $ do
      largestDivisible `shouldBe` 99554

  describe "takeWhile" $ do
    it "(/=' ')と文字列を渡すとスペースで区切られた元の単語を返す" $ do
      takeWhile (/=' ') "elepahts know how to party" `shouldBe` "elepahts"
    it "sum, filter, mapと組み合わせて10000より小さいすべての奇数の平方数の和を求める" $ do
      (sum . takeWhile (<10000) . filter odd $ map (^2) [1..]) `shouldBe` 166650
    it "等価なリスト内包表記と比較する" $ do
      (sum . takeWhile (<10000) $ [x| x <- [n^2 | n <- [1..]], odd x]) `shouldBe` 166650

  describe "chain" $ do
    it "20を渡すとコラッツ列[20,10,5,16,8,4,2,1]を返す" $ do
      chain 20 `shouldBe` [20,10,5,16,8,4,2,1]

  describe "numLongChains" $ do
   it "コラッツ列長が15より大きくなる数が1から100まででいくつあるかをもとめる" $ do
    numLongChains `shouldBe` 66 




