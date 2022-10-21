{- | Intro



            TDD & TDD sont dans un bateau

           Une expérience d'Arnaud Bailly

                        pour

                  DevFest Nantes 2022
                   21 novembre 2022

-}








{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Basement.Bounded (Zn (..), zn)
import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Maybe (fromJust, isJust)
import GHC.TypeLits (KnownNat)
import Test.Hspec (Spec, describe, it, shouldBe, hspec)
import Test.QuickCheck (
    Arbitrary (arbitrary),
    Gen,
    Positive (getPositive),
    Property,
    arbitraryBoundedEnum,
    arbitraryPrintableChar,
    choose,
    counterexample,
    elements,
    forAll,
    frequency,
    suchThat,
    tabulate, oneof
 )
import Text.Parsec (Parsec, char, count, digit, runParser, (<|>))
import Text.Printf (printf)
import Basement.Compat.Natural (Natural)
import Data.String (IsString)
import Test.Hspec.QuickCheck (prop)

-- * Test-Driven Development

--
-- The adventures of the French /Numéro de Sécurité Sociale/ aka. INSEE Number:
-- Check-out https://en.wikipedia.org/wiki/INSEE_code for more details
--
-- Business rules:
--
--  * 1 digit for gender
--  * 2 digits for year of birth
--  * 2 digits for month of birth
--  * 2 digits for department
--  * 3 digits for city code
--  * 3 digits for serial number
--  * 2 digits for control key
--
-- Plus some more "interesting" Special cases: Foreign born, persons, overseas
-- departments...

-- ** Take 1: Example-Based TDD

validateINSEE :: INSEE1 -> Bool

-- ** Test-Driving Valid INSEE Number

--
-- Following classical TDD practice, we drive the implementation of the INSEE
-- number validation logic using various test cases.
validateINSEESpec :: Spec
validateINSEESpec = do

    it "returns True given a valid INSEE Number" $
        validateINSEE "223115935012322" `shouldBe` True

    it "must have the right length" $ do
        validateINSEE "2230" `shouldBe` False
        validateINSEE "2230159350123221" `shouldBe` False

    it "first character must be 1 or 2" $
        validateINSEE "323115935012322" `shouldBe` False

    it "characters at index 2 and 3 represent year" $
        validateINSEE "2ab115935012322" `shouldBe` False

    it "characters at index 4 and 5 represent month" $ do
        validateINSEE "223ab5935012322" `shouldBe` False
        validateINSEE "223145935012322" `shouldBe` False
        validateINSEE "223005935012322" `shouldBe` False

    it "characters at index 6 and 7 represent department" $ do
        validateINSEE "22311xx35012322" `shouldBe` False
        validateINSEE "223119635012322" `shouldBe` False

    it "characters at index 6 and 7 contain 99 for a foreign-born person" $
        validateINSEE "200029923123486" `shouldBe` True

    it "characters 8, 9, and 10 represent city or country code" $
        validateINSEE "2231159zzz12322" `shouldBe` False

    it "characters 11, 12, and 13 represent an order" $
        validateINSEE "2231159123zzz22" `shouldBe` False

    it "characters 14 and 15 represent a control key" $ do
        validateINSEE "223115935012321" `shouldBe` False

-- ** Control Key "Algorithm"

computeKey :: Integer -> Integer
computeKey n =
    let r = n `mod` 97
     in 97 - r

{- | INSEE number is simply a string which is "expected" to respect the specific
 structure
-}
newtype INSEE1 = INSEE1 String
    deriving newtype (Eq, Show, IsString)

--

-- ** Validation function

--
validateINSEE (INSEE1 [gender, year1, year2, month1, month2, dept1, dept2, com1, com2, com3, order1, order2, order3, key1, key2]) =
    validateGender gender
        && validateYear [year1, year2]
        && validateMonth [month1, month2]
        && validateDepartment [dept1, dept2]
        && validateCityOrCountry [com1, com2, com3]
        && validateOrder [order1, order2, order3]
        && validateKey [gender, year1, year2, month1, month2, dept1, dept2, com1, com2, com3, order1, order2, order3] [key1, key2]
validateINSEE _ = False
--

-- *** Detailed validation functions

--
validateGender :: Char -> Bool
validateGender gender = gender == '1' || gender == '2'

validateYear :: String -> Bool
validateYear = all isDigit

validateMonth :: String -> Bool
validateMonth month =
    maybe False (\m -> m <= 12 && m > 0) (readNumber month)

{- | Validate department number
 We do not take into account the interesting but gory details of overseas departments
 or the case of persons born in Algeria, Morocco or Tunisia before 1962:
 Check https://www.previssima.fr/actualite/numero-de-securite-sociale-quelle-signification.html (in French)
-}
validateDepartment :: String -> Bool
validateDepartment dept =
    maybe False (\m -> m <= 95 && m > 0 || m == 99) (readNumber dept)

{- |
 We don't check the actual code designates a real city or country but of course
 we should.
-}
validateCityOrCountry :: String -> Bool
validateCityOrCountry commune =
    isJust (readNumber commune)

validateOrder :: String -> Bool
validateOrder order =
    isJust (readNumber order)

validateKey :: String -> String -> Bool
validateKey nir key =
    case (readNumber key, readNumber nir) of
        (Just k, Just n) ->
            computeKey n == k
        _ -> False
--

-- *** Utility Functions

--
readNumber :: String -> Maybe Integer
readNumber s =
    case reads s of
        [(m, [])] -> Just m
        _ -> Nothing
--

-- * Take 2: Type-Driven Development

{- | ** Build a Valid `INSEE` number from a `String`

 ''Parse, Don't Validate''

 Rather than having to validate an INSEE number upon each use because
 it's actually just a `String` in the `INSEE1` case, we guarantee by
 construction the resulting type, if it exists, is valid.
-}
makeINSEE :: String -> Either String INSEE

{- | ** Fundamental Isomorphism Property

 We check the functions transforming a `String` into an `INSEE` and back are
 isomorphic. This effectively gives us a compact property "triangulating" over the
 implementation space of the various components:

  * The `INSEE` type proper, into which we can introduce mistakes,
  * The `inseeParser` which is used to transform a `String` to an `INSEE`,
  * The `pretty` function for an `INSEE` which transforms it back to a `String`.
-}
parseIsInverseToPrettyPrint :: INSEE -> Property
parseIsInverseToPrettyPrint insee =
    let prettyInsee = pretty insee
        parsedInsee = makeINSEE prettyInsee
     in parsedInsee == Right insee &
         counterexample ("pretty = " <> prettyInsee <> "\n, parsed = " <> show parsedInsee)  &
         tabulate "Year" [yearRange prettyInsee]

{- | A somewhat less naive and much more self-descriptive INSEE code.
 It's a data  structure containing fields for the various constituents of an INSEE code.
 It is /correct by construction/ hence it's not possible to define an invalid value in
 our code.
-}
data INSEE = INSEE
    { gender :: Gender
    , year :: Year
    , month :: Month
    , dept :: Department
    , commune :: Commune
    , order :: Order
    }
    deriving (Eq, Show)

-- ** Sub-structure of INSEE1

-- | Gender is a 2-values enumerated type.
data Gender = M | F
    deriving (Eq, Show)

{- | Year is a number between 00 and 99.
 We use a /type-level/ finite numbers that totally constrains the possible
 values.
-}
newtype Year = Year (Zn 100)
    deriving (Eq, Show)

data Month = Jan | Fev | Mar | Apr | Mai | Jun | Jui | Aou | Sep | Oct | Nov | Dec
    deriving (Eq, Show, Enum, Bounded)

{- | Department is a so-called /Algebraic Data types/.
 We clearly distinguish the case of a "standard" department code and the case of a
 foreign-born person. Note this makes it easy to add other cases covering overseas
 departments, more complex country code or code for people born  in former colonies.
-}
data Department
    = Dept (Zn 96)
    | Foreign
    deriving (Eq, Show)

newtype Commune = Commune (Zn 1000)
    deriving (Eq, Show)

newtype Order = Order (Zn 1000)
    deriving (Eq, Show)

newtype Key = Key (Zn 100)
    deriving (Eq, Show)

-- | *** Parser implementation
makeINSEE maybeINSEENumber =
    first show $ runParser inseeParser () "" maybeINSEENumber
  where
    inseeParser :: Parsec String () INSEE
    inseeParser = do
        gender <- char '1' $> M <|> char '2' $> F
        year <- Year . zn  <$> integerDigits 2
        month <- toEnum . fromIntegral <$> integerDigits 2
        dept <- do
            s <- count 2 digit
            case readNumber s of
                Just d
                    | d < 96 -> pure $ Dept (fromInteger d)
                    | d == 99 -> pure Foreign
                    | otherwise -> fail ("can't parse " <> s <> " as a department")
                Nothing -> fail ("can't parse " <> s <> " as a department")
        commune <- Commune . zn  <$> integerDigits 3
        order <- Order . zn  <$> integerDigits 3
        key <- Key . zn  <$> integerDigits 2
        let insee = INSEE{..}
        if computeINSEEKey insee == key
            then pure insee
            else fail ("key '" <> pretty key <> "' is invalid")

integerDigits :: Int -> Parsec String () Natural
integerDigits numDigits = do
    s <- count numDigits digit
    maybe (fail $ "can't parse " <> s <> " as a number") (pure . fromInteger) $ readNumber s

-- | ** INSEE Generator
instance Arbitrary INSEE where
    arbitrary =
        INSEE
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

-- *** Sub-structure Generators

instance Arbitrary Gender where
    arbitrary = elements [M, F]

instance Arbitrary Year where
    arbitrary = Year <$> someZn

instance Arbitrary Month where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Department where
    arbitrary = frequency [(9, Dept <$> someZn), (1, pure Foreign)]

instance Arbitrary Commune where
    arbitrary = Commune <$> someZn

instance Arbitrary Order where
    arbitrary = Order <$> someZn

-- *** Utility function

someZn :: (KnownNat k) => Gen (Zn k)
someZn = zn . fromInteger . getPositive <$> (arbitrary :: Gen (Positive Integer))

-- ** 'Pretty-Printing' of an INSEE number

class Pretty a where
    pretty :: a -> String

-- *** Base implementation

instance Pretty INSEE where
    pretty insee = prettyBase insee <> pretty (computeINSEEKey insee)

prettyBase :: INSEE -> String
prettyBase (INSEE gender an month dept comun order) =
    pretty gender
        <> pretty an
        <> pretty month
        <> pretty dept
        <> pretty comun
        <> pretty order
-- *** Sub-structure Implementation

instance Pretty Gender where
    pretty F = "2"
    pretty M = "1"

instance Pretty Year where
    pretty (Year n) = printf "%02d" (unZn n)

instance Pretty Month where
    pretty m = printf "%02d" (fromEnum m)

instance Pretty Department where
    pretty (Dept d) = printf "%02d" (unZn d)
    pretty Foreign = "99"

instance Pretty Commune where
    pretty (Commune c) = printf "%03d" (unZn c)

instance Pretty Order where
    pretty (Order s) = printf "%03d" (unZn s)

instance Pretty Key where
    pretty (Key c) = printf "%02d" (unZn c)

-- *** Compute Key

-- By construction the INSEE number is correct hence the `Nothing` case
-- is impossible.
computeINSEEKey :: INSEE -> Key
computeINSEEKey insee =
    fromJust $ Key . fromInteger . computeKey <$> readNumber (prettyBase insee)

{- | * Mutation-Based Property Testing

 We notice the "naive" TDD test cases enumerate various cases of pathological INSEE numbers. We can leverage
 this idea to to build the `makeINSEE` function through a /property/: This function is correct if it accepts
 all valid INSEE numbers /and/ reject all invalid ones.

 Our /parseIsInverseToPrettyPrint/ property asserts the first proposition, we check the second by introducing
 /mutations/ into an otherwise valid number.
-}
inseeValidatorKillsNonViableMutants :: Property
inseeValidatorKillsNonViableMutants =
    forAll arbitrary $ \insee ->
        forAll nonViableMutant $ \mutation ->
            let mutant =  mutation `mutate` insee
                parsedInsee = makeINSEE mutant
             in isLeft parsedInsee
                    & tabulate "Mutation" [takeWhile (not . isSpace) $ show mutation]
                    & counterexample ("INSEE  = " <> pretty insee)
                    & counterexample ("Mutant = " <> mutant)
                    & counterexample ("Result = " <> show parsedInsee)

data Mutation = MutateYear {position :: Int, character :: Char}
  | MutateKey{key :: Key }
    deriving (Eq, Show)

nonViableMutant :: Gen Mutation
nonViableMutant = oneof [ genMutantForYear,  genMutantForKey]
 where
   genMutantForYear =  do
    position <- choose (1, 2)
    character <- arbitraryPrintableChar `suchThat` (not . isDigit)
    pure $ MutateYear{position, character}
   genMutantForKey =
     MutateKey . Key  <$> someZn

mutate :: Mutation -> INSEE -> String
mutate MutateYear{position, character} insee =
    let (prefix, suffix) = splitAt position (pretty insee)
     in prefix ++ character : tail suffix
mutate MutateKey{key} insee =
    let prefix = take 13 (pretty insee)
     in prefix ++ pretty key

yearRange :: String -> String
yearRange insee =
  let tens = fromJust (readNumber $ take 2 $ drop 1 insee) `div` 10
  in show (tens * 10, (tens +1) * 10)

-- * Conclusion

{-

 * The 'T' in TDD does not always mean 'Test'

 * Examples are good but are just one technique

 * And by the way, TDD is not about /Testing/
-}


-- * Tools

main :: IO ()
main = hspec $ describe "INSEE" $ do
  describe "Unit tests"
    validateINSEESpec
  describe "Properties" $ do
    prop "parse is inverse to print" parseIsInverseToPrettyPrint
    prop "mutants are killed" inseeValidatorKillsNonViableMutants
