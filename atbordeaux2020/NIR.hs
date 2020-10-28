{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module NIR where

{-

           TDD & TDD sont dans un bateau

           Une expérience d'Arnaud Bailly

                        pour

               Agile Tour Bordeaux 2020
                   29 octobre 2020

-}

import Basement.Bounded
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char
import GHC.TypeLits
import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import Text.Parsec

-- * Numéro de Sécurité Sociale

-- Un type naïf pour les numéros de sécurité sociale
-- voir https://www.ameli.fr/loire-atlantique/assure/droits-demarches/principes/numero-securite-sociale
newtype NIR1 = NIR1 String
  deriving (Eq, Show)

valideNIR :: NIR1 -> Bool
valideNIR (NIR1 [sexe, annee1, annee2, mois1, mois2, dept1, dept2, com1, com2, com3, serie1, serie2, serie3, cle1, cle2]) =
  valideSexe sexe
    && valideAnnee [annee1, annee2]
    && valideMois [mois1, mois2]
    && valideDepartement [dept1, dept2]
    && valideCommunePays [com1, com2, com3]
    && valideNumeroSerie [serie1, serie2, serie3]
    && valideCle [sexe, annee1, annee2, mois1, mois2, dept1, dept2, com1, com2, com3, serie1, serie2, serie3] [cle1, cle2]
valideNIR _ = False

valideSexe :: Char -> Bool
valideSexe sexe = (sexe == '1' || sexe == '2')

valideAnnee :: String -> Bool
valideAnnee annee = all isDigit annee

readNumber :: String -> Maybe Integer
readNumber s =
  case (reads s) of
    [(m, [])] -> Just m
    _ -> Nothing

valideMois :: String -> Bool
valideMois mois =
  maybe False (\m -> m <= 12 && m > 0) (readNumber mois)

-- on ne traitera pas des exceptions (intéressantes !) pour les DOM-TOM
-- et les personnes nées en Algérie, Maroc et Tunisie avant 1962:
-- voir https://www.previssima.fr/actualite/numero-de-securite-sociale-quelle-signification.html
valideDepartement :: String -> Bool
valideDepartement dept =
  maybe False (\m -> (m <= 95 && m > 0) || m == 99) (readNumber dept)

-- On ne vérifiera pas que le code commune ou pays est reel
valideCommunePays :: String -> Bool
valideCommunePays commune =
  isJust (readNumber commune)

valideNumeroSerie :: String -> Bool
valideNumeroSerie serie =
  isJust (readNumber serie)

calculCle :: Integer -> Integer
calculCle n =
  let r = n `mod` 97
   in 97 - r

valideCle :: String -> String -> Bool
valideCle nir cle =
  case (readNumber cle, readNumber nir) of
    (Just k, Just n) ->
      calculCle n == k
    _ -> False

valideNIRSpec :: Spec
valideNIRSpec = describe "NIR Valide" $ do
  let unNIRValide = NIR1 "223115935012322"
      tropCourt = NIR1 "2230"
      sexeIncorrect = NIR1 "323115935012322"
      annéeIncorrecte = NIR1 "2ab115935012322"
      moisIncorrecte = NIR1 "223ab5935012322"
      moisIncorrecte2 = NIR1 "223145935012322"
      moisIncorrecte3 = NIR1 "223005935012322"
      deptIncorrect = NIR1 "22311xx35012322"
      deptIncorrect2 = NIR1 "223119635012322"
      personneNeeEnIndonesie = NIR1 "200029923123486"
      communeInvalide = NIR1 "2231159zzz12322"
      serieInvalide = NIR1 "2231159123zzz22"
      cléDeContrôleInvalide = NIR1 "223115935012321"

  it "a le bon nombre de caractères" $ do
    valideNIR tropCourt `shouldBe` False

  it "le premier caractère est 1 ou 2" $ do
    valideNIR sexeIncorrect `shouldBe` False

  it "les caractères 2 et 3 représentent l'année de naissance sur 2 chiffres" $ do
    valideNIR annéeIncorrecte `shouldBe` False

  it "les caractères 4 et 5 représentent le mois de naissance sur 2 chiffres" $ do
    valideNIR moisIncorrecte `shouldBe` False
    valideNIR moisIncorrecte2 `shouldBe` False
    valideNIR moisIncorrecte3 `shouldBe` False

  it "les caractères 6 et 7 représentent le code département sur 2 chiffres" $ do
    valideNIR deptIncorrect `shouldBe` False
    valideNIR deptIncorrect2 `shouldBe` False

  it "les caractères 6 et 7 valent 99 pour une naissance à l'étranger" $ do
    valideNIR personneNeeEnIndonesie `shouldBe` True

  it "les caractères 8,9 et 10 representent un code commune ou pays sur 3 chiffres" $ do
    valideNIR communeInvalide `shouldBe` False

  it "les caractères 11,12 et 13 representent un numéro de série sur 3 chiffres" $ do
    valideNIR serieInvalide `shouldBe` False

  it "les caractères 14 et 15 representent une clé de contrôle sur 2 chiffres" $ do
    valideNIR cléDeContrôleInvalide `shouldBe` False
    valideNIR unNIRValide `shouldBe` True

-- Un type moins naïf pour les NIR
-- NIR est correct par construction (en première approximation en tout cas...)
data NIR = NIR
  { sexe :: Sexe,
    annee :: Annee,
    mois :: Mois,
    dept :: Departement,
    commune :: Commune,
    serie :: Serie
  }
  deriving (Eq, Show)

data Sexe = M | F
  deriving (Eq, Show)

newtype Annee = Annee (Zn 100)
  deriving (Eq, Show)

data Mois = Jan | Fev | Mar | Apr | Mai | Jun | Jui | Aou | Sep | Oct | Nov | Dec
  deriving (Eq, Show, Enum, Bounded)

data Departement
  = Dept (Zn 96)
  | Etranger
  deriving (Eq, Show)

newtype Commune = Commune (Zn 1000)
  deriving (Eq, Show)

newtype Serie = Serie (Zn 1000)
  deriving (Eq, Show)

-- ''Parse, Don't Validate''
-- plutôt que de devoir vérifier à chaque utilisation la validité d'un NIR,
-- ce qui serait le cas avec la représentation NIR1, on garantit par construction
-- que le NIR est valide
makeNIR :: String -> Either String NIR
makeNIR peutEtreUnNir =
  bimap show id $ runParser nirParser () "" peutEtreUnNir

-- squelette de parser pour transformer une chaine en NIR
nirParser :: Parsec String () NIR
nirParser = error "not implemented"

instance Arbitrary Sexe where
  arbitrary = elements [M, F]

someZn :: (KnownNat k) => Gen (Zn k)
someZn = zn . fromIntegral @Int . getPositive <$> arbitrary

instance Arbitrary Annee where
  arbitrary = Annee <$> someZn

instance Arbitrary Mois where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Departement where
  arbitrary = frequency [(9, Dept <$> someZn), (1, pure Etranger)]

instance Arbitrary Commune where
  arbitrary = Commune <$> someZn

instance Arbitrary Serie where
  arbitrary = Serie <$> someZn

instance Arbitrary NIR where
  arbitrary =
    NIR
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- isomorphisme parser/pretty-printer
parseEstInverseDePrint :: NIR -> Property
parseEstInverseDePrint nir =
  let prettyNir = prettyPrint nir
   in counterexample prettyNir $ makeNIR prettyNir == Right nir

-- squelette de pretty-printer pour un NIR
prettyPrint :: NIR -> String
prettyPrint = error "not implemented"
