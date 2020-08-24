module NonEmptyTest exposing (..)

import Dict exposing (Dict)
import Expect
import NonEmpty exposing (NonEmpty(..))
import Random
import Test exposing (Test)


exampleTest : Test
exampleTest =
    let
        input =
            NonEmpty 1 [ 2, 3 ]

        ( actual1, _ ) =
            Random.step (NonEmpty.sample input) (Random.initialSeed 0)

        ( actual2, _ ) =
            Random.step (NonEmpty.sample input) (Random.initialSeed 3)

        ( actual3, _ ) =
            Random.step (NonEmpty.sample input) (Random.initialSeed 1)
    in
    Test.describe "exampleTest"
        [ Test.test "sample1" <| always <| Expect.equal actual1 1
        , Test.test "sample2" <| always <| Expect.equal actual2 2
        , Test.test "sample3" <| always <| Expect.equal actual3 3
        ]


generateSample : Int -> Random.Generator Int
generateSample size =
    let
        head =
            0

        tail =
            List.range 1 (size - 1)

        list =
            NonEmpty head tail
    in
    NonEmpty.sample list


generateSampleCounts : Int -> Int -> Dict Int Int -> Random.Generator (Dict Int Int)
generateSampleCounts numSamples numBuckets counts =
    if numSamples == 0 then
        Random.constant counts

    else
        let
            gotFirstSample sample =
                let
                    updateCount maybeCount =
                        case maybeCount of
                            Just count ->
                                Just <| count + 1

                            Nothing ->
                                Just 1

                    newCounts =
                        Dict.update sample updateCount counts
                in
                generateSampleCounts (numSamples - 1) numBuckets newCounts
        in
        generateSample numBuckets |> Random.andThen gotFirstSample



-- Generate a bunch of lists and verify that the sampler samples uniformly from them


randomTest : Test
randomTest =
    let
        numSamples =
            10000

        numBuckets =
            10

        ( counts, _ ) =
            Random.step (generateSampleCounts numSamples numBuckets Dict.empty) (Random.initialSeed 42)

        uniformDistribution =
            1.0 / numBuckets

        actualDistribution count =
            toFloat count / numSamples

        toTest : ( Int, Int ) -> Test
        toTest ( bucket, count ) =
            Test.test ("bucket " ++ String.fromInt bucket ++ " should have proportion ~ 1 / numBuckets") <|
                \_ ->
                    Expect.within (Expect.Relative 0.1) uniformDistribution (actualDistribution count)

        tests : List Test
        tests =
            Dict.toList counts |> List.map toTest
    in
    Test.describe "randomTest"
        [ Test.test ("should have " ++ String.fromInt numBuckets ++ " tests") <| \_ -> Expect.equal numBuckets (List.length tests)
        , Test.describe "should be close to uniform" tests
        ]


shuffleTest : Test
shuffleTest =
    Test.describe "testShuffle"
        [ Test.test "testShuffle1" <|
            \_ ->
                Expect.equal (NonEmpty 3 [ 2, 4, 1, 6, 7, 9, 5, 8, 0 ]) (Random.step (NonEmpty.shuffle (NonEmpty 0 (List.range 1 9))) (Random.initialSeed 42) |> Tuple.first)
        , Test.test "testShuffle2" <|
            \_ ->
                Expect.equal (NonEmpty 2 [ 3, 5, 4, 6, 7, 9, 1, 8, 0 ]) (Random.step (NonEmpty.shuffle (NonEmpty 0 (List.range 1 9))) (Random.initialSeed 43) |> Tuple.first)
        ]
