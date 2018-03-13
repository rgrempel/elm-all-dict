module Prelude exposing (Comparable, Equatable)

{-| Provides a number of basic capabilities.
-}


type alias Comparable t a =
    { t | compare : a -> a -> Order }


type alias Equatable t a =
    { t | equals : a -> a -> Bool }


type alias Showable t a =
    { t | show : a -> String }


comparable : (a -> a -> Order) -> Comparable {} a
comparable ord =
    { compare = ord }


equatable : (a -> a -> Bool) -> Equatable {} a
equatable equals =
    { equals = equals }


equalsFromComparable : Comparable t a -> a -> a -> Bool
equalsFromComparable comparable a b =
    comparable.compare a b == EQ


withDefaultEquals : Comparable {} a -> Comparable (Equatable {} a) a
withDefaultEquals comparable =
    { compare = comparable.compare
    , equals = equalsFromComparable comparable
    }
