-- * Preface
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module NIR where

-- * Intro
{-

           TDD & TDD sont dans un bateau

           Une expérience d'Arnaud Bailly

                        pour

               Agile Tour Bordeaux 2020
                   29 octobre 2020

-}

-- ** Imports indispensables

import Basement.Bounded ( zn, Zn(..) )
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char ( isDigit )
import Data.Maybe ( fromJust, isJust )
import GHC.TypeLits ( KnownNat )
import Test.Hspec ( describe, it, shouldBe, Spec )
import Test.QuickCheck
    ( arbitraryBoundedEnum,
      elements,
      frequency,
      counterexample,
      Arbitrary(arbitrary),
      Gen,
      Positive(getPositive),
      Property )
import Text.Parsec ( char, digit, count, (<|>), runParser, Parsec )
import Text.Printf ( printf )

-- * Test-Driven Development

-- Les aventures du Numéro de Sécurité Sociale ou NIR:
-- voir https://www.ameli.fr/loire-atlantique/assure/droits-demarches/principes/numero-securite-sociale
-- pour les détails

-- ** Une première représentation "naïve"
--
-- Un NIR est simplement une chaîne de caractères qui a une structure particulière
newtype NIR1 = NIR1 String
  deriving (Eq, Show)

-- ** Tests pour la validation d'un NIR1

-- En Test-Driven Development, on construit un ensemble de tests pour "explorer l'espace"
-- des différents cas de validité ou d'invalidité d'un NIR
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

-- ** Règle de calcul de la clé

calculCle :: Integer -> Integer
calculCle n =
  let r = n `mod` 97
   in 97 - r

-- ** Fonction de validation

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


-- *** Détail des fonctions de validation de chaque segment
valideSexe :: Char -> Bool
valideSexe sexe = (sexe == '1' || sexe == '2')

valideAnnee :: String -> Bool
valideAnnee annee = all isDigit annee

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

valideCle :: String -> String -> Bool
valideCle nir cle =
  case (readNumber cle, readNumber nir) of
    (Just k, Just n) ->
      calculCle n == k
    _ -> False


-- *** Fonction utilitaire
readNumber :: String -> Maybe Integer
readNumber s =
  case (reads s) of
    [(m, [])] -> Just m
    _ -> Nothing


-- * Type-Driven Development
--
-- Un type moins naïf pour les NIR : le NIR est correct par construction, les
-- contraintes de correction sont "embarquées" dans le type et sa structure
data NIR = NIR
  { sexe :: Sexe,
    annee :: Annee,
    mois :: Mois,
    dept :: Departement,
    commune :: Commune,
    serie :: Serie
  }
  deriving (Eq, Show)

-- ** Sous-structure de NIR1

-- | Le sexe est un type énuméré simple qui peut prendre 2 valeurs
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

newtype Cle = Cle (Zn 100)
  deriving (Eq, Show)

-- ** Construire un NIR Valide à partir d'une chaîne

-- ''Parse, Don't Validate''
-- plutôt que de devoir vérifier à chaque utilisation la validité d'un NIR,
-- ce qui serait le cas avec la représentation NIR1, on garantit par construction
-- que le NIR est valide
makeNIR :: String -> Either String NIR
makeNIR peutEtreUnNir =
  bimap show id $ runParser nirParser () "" peutEtreUnNir

-- ** Propriétés fondamentale: Isomorphisme

-- isomorphisme parser/pretty-printer
-- On "triangule" pour garantir la correction:
--
--  * du NIR proprement dit (après tout, on peut faire des erreurs en définissant les types)
--  * de l'interprétation d'une chaîne de caractères quelconque en NIR valide
--  * de la représentation d'un NIR en chaîne de caractères
--
parseEstInverseDePrint :: NIR -> Property
parseEstInverseDePrint nir =
  let prettyNir = pretty nir
      parsedNir = makeNIR prettyNir
   in counterexample ("pretty = " <> prettyNir <> "\n, parsed = " <> show parsedNir) $
        parsedNir == Right nir

--- *** Implémentation du parser

nirParser :: Parsec String () NIR
nirParser = do
  sexe <- char '1' *> pure M <|> char '2' *> pure F
  annee <- Annee . zn . fromInteger <$> integerDigits 2
  mois <- toEnum . fromInteger <$> integerDigits 2
  dept <- do
    s <- count 2 digit
    case readNumber s of
      Just d
        | d < 96 -> pure $ Dept (fromInteger d)
        | d == 99 -> pure Etranger
        | otherwise -> fail ("can't parse " <> s <> " as a department")
      Nothing -> fail ("can't parse " <> s <> " as a department")
  commune <- Commune . zn . fromInteger <$> integerDigits 3
  serie <- Serie . zn . fromInteger <$> integerDigits 3
  cle <- Cle . zn . fromInteger <$> integerDigits 2
  let nir = NIR {..}
  if calculCleNIR nir == cle
    then pure $ nir
    else fail ("clé " <> show cle <> " invalide")

integerDigits :: Int -> Parsec String () Integer
integerDigits numDigits = do
    s <- count numDigits digit
    maybe (fail $ "can't parse " <> s <> " as a number") pure $ readNumber s

-- ** Générateur de NIR

instance Arbitrary NIR where
  arbitrary =
    NIR
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- *** Détails des générateurs de la structure

instance Arbitrary Sexe where
  arbitrary = elements [M, F]

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

-- *** Utilitaire

someZn :: (KnownNat k) => Gen (Zn k)
someZn = zn . fromIntegral @Int . getPositive <$> arbitrary

-- ** 'Pretty-Printing' d'un NIR

class Pretty a where
  pretty :: a -> String

-- *** Implémentation de base

instance Pretty NIR where
  pretty nir =  prettyBase nir <> pretty (calculCleNIR nir)

prettyBase :: NIR -> String
prettyBase (NIR sexe an mois dept comun serie) =
  pretty sexe
    <> pretty an
    <> pretty mois
    <> pretty dept
    <> pretty comun
    <> pretty serie

-- *** Implémentation des sous-structures

instance Pretty Sexe where
  pretty F = "2"
  pretty M = "1"

instance Pretty Annee where
  pretty (Annee n) = printf "%02d" (unZn n)

instance Pretty Mois where
  pretty m = printf "%02d" (fromEnum m)

instance Pretty Departement where
  pretty (Dept d) = printf "%02d" (unZn d)
  pretty Etranger = "99"

instance Pretty Commune where
  pretty (Commune c) = printf "%03d" (unZn c)

instance Pretty Serie where
  pretty (Serie s) = printf "%03d" (unZn s)

instance Pretty Cle where
  pretty (Cle c) = printf "%02d" (unZn c)

-- *** Calcul de cle

-- par construction le type est correct donc le cas `Nothing` est
-- impossible
calculCleNIR :: NIR -> Cle
calculCleNIR nir =
  fromJust $ Cle . fromInteger . calculCle <$> readNumber (prettyBase nir)

-- * Conclusion
