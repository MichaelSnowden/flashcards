module NonEmpty exposing (NonEmpty(..), append, extend, fromList, map, sample, shuffle, sort, sortBy, toList)

import Random


type NonEmpty a
    = NonEmpty a (List a)


fromList : List a -> Maybe (NonEmpty a)
fromList list =
    case list of
        head :: tail ->
            Just (NonEmpty head tail)

        [] ->
            Nothing


toList : NonEmpty a -> List a
toList (NonEmpty head tail) =
    head :: tail


map : (a -> b) -> NonEmpty a -> NonEmpty b
map f (NonEmpty head tail) =
    NonEmpty (f head) (List.map f tail)


append : List a -> a -> NonEmpty a
append list x =
    case list of
        [] ->
            NonEmpty x []

        head :: tail ->
            NonEmpty head (tail ++ [ x ])


extend : NonEmpty a -> List a -> NonEmpty a
extend (NonEmpty head tail) list =
    NonEmpty head (tail ++ list)


insertSorted : (a -> comparable) -> a -> List a -> List a -> NonEmpty a
insertSorted f x left right =
    case right of
        [] ->
            extend (append left x) right

        head :: tail ->
            if f x < f head then
                extend (append left x) right

            else
                insertSorted f x (left ++ [ head ]) tail


sortBy : (a -> comparable) -> NonEmpty a -> NonEmpty a
sortBy f (NonEmpty head tail) =
    let
        sortedTail =
            List.sortBy f tail
    in
    insertSorted f head sortedTail []


sort : NonEmpty comparable -> NonEmpty comparable
sort list =
    sortBy identity list


sampleStream : Int -> a -> a -> Random.Generator a
sampleStream numSeen old new =
    Random.int 0 (numSeen + 1)
        |> Random.map
            (\k ->
                if k == 0 then
                    new

                else
                    old
            )


sampleListAfter : Int -> NonEmpty a -> Random.Generator a
sampleListAfter numSeen (NonEmpty head tail) =
    case tail of
        [] ->
            Random.constant head

        nextHead :: nextTail ->
            sampleStream numSeen head nextHead
                |> Random.andThen
                    (\s -> sampleListAfter (numSeen + 1) (NonEmpty s nextTail))


sample : NonEmpty a -> Random.Generator a
sample =
    sampleListAfter 0


type alias Annotated a =
    { x : Float, a : a }


annotate : a -> Random.Generator (Annotated a)
annotate a =
    Random.float 0 1 |> Random.map (\x -> { x = x, a = a })


annotateList : List a -> Random.Generator (List (Annotated a))
annotateList list =
    case list of
        [] ->
            Random.constant []

        head :: tail ->
            Random.map2 (::) (annotate head) (annotateList tail)


annotateNonEmpty : NonEmpty a -> Random.Generator (NonEmpty (Annotated a))
annotateNonEmpty (NonEmpty head tail) =
    Random.map2 NonEmpty (annotate head) (annotateList tail)


shuffle : NonEmpty a -> Random.Generator (NonEmpty a)
shuffle list =
    annotateNonEmpty list |> Random.map (sortBy .x) |> Random.map (map .a)
