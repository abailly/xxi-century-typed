import           Test.Hspec

main = hspec $ do
    describe "isVegetarian" $ do
        it "is true if no dish" $ do
            isVegetarian [] `shouldBe` True
        it "is true with salad" $ do
            isVegetarian [Salad ] `shouldBe` True
        it "is true with salad and tomato" $ do
            isVegetarian [Salad, Tomato]  `shouldBe` True
        it "is false with mutton" $ do
            isVegetarian [ Mutton ] `shouldBe` False
        it "is false with mutton and vegetables" $ do
            isVegetarian [ Mutton, Tomato ]  `shouldBe` False
        it "is false with mutton and vegetables" $ do
            isVegetarian [ Salad, Mutton ]  `shouldBe` False
    describe "isPescetarian" $ do
        it "is true with any vegetarian" $ do
            isPescetarian [Salad, Tomato] `shouldBe` True
        it "is false with any meat" $ do
            isPescetarian [Mutton] `shouldBe` False
        it "is true with Shrimp" $ do
            isPescetarian [Salad, Shrimp, Tomato] `shouldBe` True
        it "is false with Mutton,Shrimp" $ do
            isPescetarian [Mutton,Shrimp] `shouldBe` False
    describe "removeOnion" $ do
        it "removes onion from kebab" $ do
            removeOnion [Tomato, Onion] `shouldBe` [Tomato]
        it "removes all onions from kebab" $ do
            removeOnion [Onion, Tomato, Onion] `shouldBe` [Tomato]
    describe "doubleCheese" $ do
        it "does nothing without cheese" $ do
            doubleCheese [Tomato, Onion]  `shouldBe` [Tomato, Onion]
        it "double the cheese" $ do
            doubleCheese [Tomato,Cheese,Onion] `shouldBe` [Tomato, Cheese, Cheese, Onion]

data Ingredient = Salad | Tomato | Mutton | Shrimp | Onion | Cheese
      deriving (Show, Eq)

fish = [Shrimp]
meat = [Mutton]
vegetable = [Tomato,Salad,Onion,Cheese]

isFish = (`elem` fish)
isMeat = (`elem` meat)
isVegetable = (`elem` vegetable)

isVegetarian = all isVegetable

isPescetarian = all (not . isMeat)

removeOnion []         = []
removeOnion (Onion:xs) = removeOnion xs
removeOnion (x:xs)     = (x:removeOnion xs)

doubleCheese = concatMap doubleTheCheese
    where
    doubleTheCheese Cheese = [Cheese, Cheese]
    doubleTheCheese x      = [x]
