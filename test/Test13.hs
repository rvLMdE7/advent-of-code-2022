{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test13 where

import Data.Proxy (Proxy(Proxy))
import Test.QuickCheck.Classes (Laws, ordLaws)
import Test.QuickCheck.Classes qualified as Laws
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (Arbitrary)
import Test.Tasty.QuickCheck qualified as Tasty.Quick
import Text.InterpolatedString.Perl6 (qq)

import Day13 (Input(In))
import Day13 qualified


deriving newtype instance Arbitrary a => Arbitrary (Input a)

checkLaws :: String -> Laws -> TestTree
checkLaws desc laws = Tasty.testGroup title $ do
    (name, prop) <- Laws.lawsProperties laws
    pure $ Tasty.Quick.testProperty name prop
  where
    title = [qq|{Laws.lawsTypeclass laws} ($desc)|]


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [propTests, unitTests]


propTests :: TestTree
propTests = Tasty.testGroup "property tests" [lawTests]

lawTests :: TestTree
lawTests = Tasty.testGroup "law tests"
    [ checkLaws "Input Int" $ ordLaws $ Proxy @(Input Int) ]


unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "example" $ Day13.part1 example @?= 13 ]


example :: [(Input Int, Input Int)]
example =
    [ ( In $ fmap Left [1,1,3,1,1]
      , In $ fmap Left [1,1,5,1,1] )

    , ( In [Right $ In [Left 1], Right $ In [Left 2, Left 3, Left 4]]
      , In [Right $ In [Left 1], Left 4] )

    , ( In [Left 9]
      , In [Right $ In [Left 8, Left 7, Left 6]] )

    , ( In [Right $ In [Left 4, Left 4], Left 4, Left 4]
      , In [Right $ In [Left 4, Left 4], Left 4, Left 4, Left 4] )

    , ( In $ fmap Left [7,7,7,7]
      , In $ fmap Left [7,7,7] )

    , ( In []
      , In [Left 3] )

    , ( In [Right $ In [Right $ In []]]
      , In [Right $ In []] )

    , ( In [ Left 1
           , Right $ In
               [ Left 2
               , Right $ In
                   [ Left 3
                   , Right $ In [Left 4, Right $ In $ fmap Left [5,6,7]] ]]
           , Left 8
           , Left 9 ]
      , In [ Left 1
           , Right $ In
               [ Left 2
               , Right $ In
                   [ Left 3
                   , Right $ In [Left 4, Right $ In $ fmap Left [5,6,0]] ]]
           , Left 8
           , Left 9 ])]
