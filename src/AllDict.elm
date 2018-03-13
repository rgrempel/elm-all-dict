module AllDict
    exposing
        ( AllDict
        , AllDictModule
        , Comparable
        , allDictModule
        , comparable
        , empty
        , singleton
        , insert
        , update
        , get
        , remove
        , member
        , eq
        , size
        , isEmpty
        , filter
        , partition
        , foldl
        , foldr
        , map
        , union
        , intersect
        , diff
        , keys
        , values
        , toList
        , fromList
        )

{-| A dictionary mapping unique keys to values. This dictionary can use any type as a key.
In the core-provided Dict, keys can only be comparable.

AllDict allows you to provide a custom operation for hashing keys. The ord function has a type signature of `k -> comparable`.

It's possible to define the ord function in such a way that AllDict can actually function as a DefaultDict.

An example of this might be

    ord : Action -> Int
    ord action =
        case action of
            Run ->
                0

            Hide ->
                1

            StandStill ->
                2

            _ ->
                3

This will default any Action in the dictionary to 3. You can then set a default like so:

    myDict : AllDict Action String Int
    myDict =
        AllDict.fromList
            ord
            [ ( Noop, "Do nothing" ) ]


    -- equal to `"Do nothing"`
    -- as `ord` maps anything outside of Run, Hide, StandStill
    -- to the same hash

    eat =
        myDict
            |> AllDict.get Eat

Insert, remove, and query operations all take *O(log n)* time. AllDictionary
equality with `(==)` is unreliable and should not be used. Instead, use AllDict.eq for element-wise comparisons,
and AllDict.fullEq for a full comparison


# Types

@docs AllDict
@docs Comparable, comparable
@docs AllDictModule, allDictModule


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size, eq


# Combine

@docs union, intersect, diff


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

-- This is a hack
-- As AllDict uses types with the same names as those used
-- internally by Elm-Core's Dict, when the runtime tries to
-- shortcut and pretty-print the Dict (as the ctor of the
-- objects match), it will cause an error if Dict has not been
-- imported at least somewhere in the program.
-- So, we import it here - and get prettyprinting for free!

import Dict as DeadDict
import Basics exposing (..)
import Maybe exposing (..)
import List exposing (..)
import Debug
import String


{-| Represents the entire module, specialized for a particular type.
So, you can do things like:

    type Id
        = Id Int

    compareId : Id -> Id -> Order
    compareId (Id id1) (Id id2) =
        compara id1 id2

    comparableId : Comparable Id
    comparableId =
        comparable compareId

    allDictId : AllDictModule Id v a b
    allDictId =
        allDictModule comparableId

In principle, this should really be `type alias AllDictModule k`, and the `v a b`
types should be qualified by `forall` in each field definition. However, that
would require Rank-N types, which Elm doesn't have.

-}
type alias AllDictModule k v a b =
    { diff : AllDict k v -> AllDict k v -> AllDict k v
    , empty : AllDict k v
    , eq : AllDict k v -> AllDict k v -> Bool
    , filter : (k -> v -> Bool) -> AllDict k v -> AllDict k v
    , foldl : (k -> v -> b -> b) -> b -> AllDict k v -> b
    , foldr : (k -> v -> b -> b) -> b -> AllDict k v -> b
    , fromList : List ( k, v ) -> AllDict k v
    , get : k -> AllDict k v -> Maybe v
    , insert : k -> v -> AllDict k v -> AllDict k v
    , intersect : AllDict k v -> AllDict k v -> AllDict k v
    , isEmpty : AllDict k v -> Bool
    , keys : AllDict k v -> List k
    , map : (k -> a -> b) -> AllDict k a -> AllDict k b
    , member : k -> AllDict k v -> Bool
    , partition : (k -> v -> Bool) -> AllDict k v -> ( AllDict k v, AllDict k v )
    , remove : k -> AllDict k v -> AllDict k v
    , singleton : k -> v -> AllDict k v
    , size : AllDict k v -> Int
    , toList : AllDict k v -> List ( k, v )
    , union : AllDict k v -> AllDict k v -> AllDict k v
    , update : k -> (Maybe v -> Maybe v) -> AllDict k v -> AllDict k v
    , values : AllDict k v -> List v
    }


{-| Given a way of comparing a type, produces a version of this module specialized
for that type.
-}
allDictModule : Comparable k -> AllDictModule k v a b
allDictModule comparable =
    { diff = diff comparable
    , empty = empty
    , eq = eq comparable
    , filter = filter comparable
    , foldl = foldl
    , foldr = foldr
    , fromList = fromList comparable
    , get = get comparable
    , insert = insert comparable
    , intersect = intersect comparable
    , isEmpty = isEmpty
    , keys = keys
    , map = map
    , member = member comparable
    , partition = partition comparable
    , remove = remove comparable
    , singleton = singleton comparable
    , size = size
    , toList = toList
    , union = union comparable
    , update = update comparable
    , values = values
    }



-- BBlack and NBlack should only be used during the deletion
-- algorithm. Any other occurrence is a bug and should fail an assert.


type NColor
    = Red
    | Black
    | BBlack -- Double Black, counts as 2 blacks for the invariant
    | NBlack -- Negative Black, counts as -1 blacks for the invariant


showNColor : NColor -> String
showNColor c =
    case c of
        Red ->
            "Red"

        Black ->
            "Black"

        BBlack ->
            "BBlack"

        NBlack ->
            "NBlack"


type LeafColor
    = LBlack
    | LBBlack -- Double Black, counts as 2


showLColor : LeafColor -> String
showLColor color =
    case color of
        LBlack ->
            "LBlack"

        LBBlack ->
            "LBBlack"


{-| A dict which works with any type which lifts the type from Core's Dict
-}
type AllDict k v
    = RBNode_elm_builtin NColor k v (AllDict k v) (AllDict k v)
    | RBEmpty_elm_builtin LeafColor


{-| How can we compare keys? Note this must be consistently supplied ...
best to use ...
-}
type Comparable k
    = Comparable (k -> k -> Order)


{-| Produce a `Comparable` for a type.
-}
comparable : (k -> k -> Order) -> Comparable k
comparable =
    Comparable


{-| Create an empty dictionary using a given ord function to calculate hashes
-}
empty : AllDict k v
empty =
    RBEmpty_elm_builtin LBlack


{-| Equality!
-}
eq : Comparable k -> AllDict k v -> AllDict k v -> Bool
eq comparable first second =
    (toList first) == (toList second)


min : AllDict k v -> ( k, v )
min dict =
    case dict of
        RBNode_elm_builtin _ key value (RBEmpty_elm_builtin LBlack) _ ->
            ( key, value )

        RBNode_elm_builtin _ _ _ left _ ->
            min left

        RBEmpty_elm_builtin _ ->
            Debug.crash "(min Empty) is not defined"


max : AllDict k v -> ( k, v )
max dict =
    case dict of
        RBNode_elm_builtin _ key value _ (RBEmpty_elm_builtin _) ->
            ( key, value )

        RBNode_elm_builtin _ _ _ _ right ->
            max right

        RBEmpty_elm_builtin _ ->
            Debug.crash "(max Empty) is not defined"


getHelper : Comparable k -> k -> AllDict k v -> Maybe v
getHelper ((Comparable ord) as comparable) targetKey dict =
    case dict of
        RBEmpty_elm_builtin _ ->
            Nothing

        RBNode_elm_builtin _ key value left right ->
            case ord targetKey key of
                LT ->
                    getHelper comparable targetKey left

                EQ ->
                    Just value

                GT ->
                    getHelper comparable targetKey right


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Mouse" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : Comparable k -> k -> AllDict k v -> Maybe v
get =
    getHelper


{-| Determine if a key is in a dictionary.
-}
member : Comparable k -> k -> AllDict k v -> Bool
member comparable key dict =
    case getHelper comparable key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : AllDict k v -> Bool
isEmpty dict =
    case dict of
        RBEmpty_elm_builtin _ ->
            True

        _ ->
            False


{-| Get the number of key-value pairs in a dict
-}
size : AllDict k v -> Int
size dict =
    sizeHelp 0 dict


sizeHelp : Int -> AllDict k v -> Int
sizeHelp n dict =
    case dict of
        RBEmpty_elm_builtin _ ->
            n

        RBNode_elm_builtin _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left


ensureBlackRoot : AllDict k v -> AllDict k v
ensureBlackRoot dict =
    case dict of
        RBNode_elm_builtin Red key value left right ->
            RBNode_elm_builtin Black key value left right

        RBNode_elm_builtin Black _ _ _ _ ->
            dict

        _ ->
            dict


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : Comparable k -> k -> v -> AllDict k v -> AllDict k v
insert comparable key value dict =
    update comparable key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : Comparable k -> k -> AllDict k v -> AllDict k v
remove comparable key dict =
    update comparable key (always Nothing) dict


type Flag
    = Insert
    | Remove
    | Same


showFlag : Flag -> String
showFlag f =
    case f of
        Insert ->
            "Insert"

        Remove ->
            "Remove"

        Same ->
            "Same"


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : Comparable k -> k -> (Maybe v -> Maybe v) -> AllDict k v -> AllDict k v
update (Comparable ord) k alter dict =
    let
        up dict =
            case dict of
                RBEmpty_elm_builtin _ ->
                    case alter Nothing of
                        Nothing ->
                            ( Same, empty )

                        Just v ->
                            ( Insert, RBNode_elm_builtin Red k v empty empty )

                RBNode_elm_builtin clr key value left right ->
                    case ord k key of
                        EQ ->
                            case alter (Just value) of
                                Nothing ->
                                    ( Remove, rem clr left right )

                                Just newValue ->
                                    ( Same, RBNode_elm_builtin clr key newValue left right )

                        LT ->
                            let
                                ( flag, newLeft ) =
                                    up left
                            in
                                case flag of
                                    Same ->
                                        ( Same, RBNode_elm_builtin clr key value newLeft right )

                                    Insert ->
                                        ( Insert, balance clr key value newLeft right )

                                    Remove ->
                                        ( Remove, bubble clr key value newLeft right )

                        GT ->
                            let
                                ( flag, newRight ) =
                                    up right
                            in
                                case flag of
                                    Same ->
                                        ( Same, RBNode_elm_builtin clr key value left newRight )

                                    Insert ->
                                        ( Insert, balance clr key value left newRight )

                                    Remove ->
                                        ( Remove, bubble clr key value left newRight )

        ( flag, updatedDict ) =
            up dict
    in
        case flag of
            Same ->
                updatedDict

            Insert ->
                ensureBlackRoot updatedDict

            Remove ->
                blacken updatedDict


{-| Create a dictionary with one key-value pair.
-}
singleton : Comparable k -> k -> v -> AllDict k v
singleton comparable key value =
    insert comparable key value empty


isBBlack : AllDict k v -> Bool
isBBlack dict =
    case dict of
        RBNode_elm_builtin BBlack _ _ _ _ ->
            True

        RBEmpty_elm_builtin LBBlack ->
            True

        _ ->
            False


moreBlack : NColor -> NColor
moreBlack color =
    case color of
        Black ->
            BBlack

        Red ->
            Black

        NBlack ->
            Red

        BBlack ->
            Debug.crash "Can't make a double black node more black!"


lessBlack : NColor -> NColor
lessBlack color =
    case color of
        BBlack ->
            Black

        Black ->
            Red

        Red ->
            NBlack

        NBlack ->
            Debug.crash "Can't make a negative black node less black!"


lessBlackTree : AllDict k v -> AllDict k v
lessBlackTree dict =
    case dict of
        RBNode_elm_builtin c k v l r ->
            RBNode_elm_builtin (lessBlack c) k v l r

        RBEmpty_elm_builtin LBBlack ->
            RBEmpty_elm_builtin LBlack

        _ ->
            dict


reportRemBug : String -> NColor -> String -> String -> a
reportRemBug msg c lgot rgot =
    Debug.crash <|
        String.concat
            [ "Internal red-black tree invariant violated, expected "
            , msg
            , " and got "
            , showNColor c
            , "/"
            , lgot
            , "/"
            , rgot
            , "\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"
            ]



-- Remove the top node from the tree, may leave behind BBlacks


rem : NColor -> AllDict k v -> AllDict k v -> AllDict k v
rem c l r =
    case ( l, r ) of
        ( RBEmpty_elm_builtin _, RBEmpty_elm_builtin _ ) ->
            case c of
                Red ->
                    RBEmpty_elm_builtin LBlack

                Black ->
                    RBEmpty_elm_builtin LBBlack

                _ ->
                    Native.Debug.crash "cannot have bblack or nblack nodes at this point"

        ( RBEmpty_elm_builtin cl, RBNode_elm_builtin cr k_ v_ l_ r_ ) ->
            case ( c, cl, cr ) of
                ( Black, LBlack, Red ) ->
                    RBNode_elm_builtin Black k_ v_ l_ r_

                _ ->
                    reportRemBug "Black/LBlack/Red" c (showLColor cl) (showNColor cr)

        ( RBNode_elm_builtin cl k_ v_ l_ r_, RBEmpty_elm_builtin cr ) ->
            case ( c, cl, cr ) of
                ( Black, Red, LBlack ) ->
                    RBNode_elm_builtin Black k_ v_ l_ r_

                _ ->
                    reportRemBug "Black/Red/LBlack" c (showNColor cl) (showLColor cr)

        -- l and r are both RBNode_elm_builtins
        ( RBNode_elm_builtin cl kl vl ll rl, RBNode_elm_builtin cr kr vr lr rr ) ->
            let
                l =
                    RBNode_elm_builtin cl kl vl ll rl

                r =
                    RBNode_elm_builtin cr kr vr lr rr

                ( k, v ) =
                    max l

                l_ =
                    remove_max cl kl vl ll rl
            in
                bubble c k v l_ r



-- Kills a BBlack or moves it upward, may leave behind NBlack


bubble : NColor -> k -> v -> AllDict k v -> AllDict k v -> AllDict k v
bubble c k v l r =
    if isBBlack l || isBBlack r then
        balance (moreBlack c) k v (lessBlackTree l) (lessBlackTree r)
    else
        RBNode_elm_builtin c k v l r



-- Removes rightmost node, may leave root as BBlack


remove_max : NColor -> k -> v -> AllDict k v -> AllDict k v -> AllDict k v
remove_max c k v l r =
    case r of
        RBEmpty_elm_builtin _ ->
            rem c l r

        RBNode_elm_builtin cr kr vr lr rr ->
            bubble c k v l (remove_max cr kr vr lr rr)



-- generalized tree balancing act


balance : NColor -> k -> v -> AllDict k v -> AllDict k v -> AllDict k v
balance c k v l r =
    balance_node (RBNode_elm_builtin c k v l r)


blackish : AllDict k v -> Bool
blackish t =
    case t of
        RBNode_elm_builtin c _ _ _ _ ->
            c == Black || c == BBlack

        RBEmpty_elm_builtin _ ->
            True


balance_node : AllDict k v -> AllDict k v
balance_node t =
    let
        assemble col xk xv yk yv zk zv a b c d =
            RBNode_elm_builtin (lessBlack col) yk yv (RBNode_elm_builtin Black xk xv a b) (RBNode_elm_builtin Black zk zv c d)
    in
        if blackish t then
            case t of
                RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red yk yv (RBNode_elm_builtin Red xk xv a b) c) d ->
                    assemble col xk xv yk yv zk zv a b c d

                RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red xk xv a (RBNode_elm_builtin Red yk yv b c)) d ->
                    assemble col xk xv yk yv zk zv a b c d

                RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red zk zv (RBNode_elm_builtin Red yk yv b c) d) ->
                    assemble col xk xv yk yv zk zv a b c d

                RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red yk yv b (RBNode_elm_builtin Red zk zv c d)) ->
                    assemble col xk xv yk yv zk zv a b c d

                RBNode_elm_builtin BBlack xk xv a (RBNode_elm_builtin NBlack zk zv (RBNode_elm_builtin Black yk yv b c) d) ->
                    case d of
                        RBNode_elm_builtin Black _ _ _ _ ->
                            RBNode_elm_builtin Black yk yv (RBNode_elm_builtin Black xk xv a b) (balance Black zk zv c (redden d))

                        _ ->
                            t

                RBNode_elm_builtin BBlack zk zv (RBNode_elm_builtin NBlack xk xv a (RBNode_elm_builtin Black yk yv b c)) d ->
                    case a of
                        RBNode_elm_builtin Black _ _ _ _ ->
                            RBNode_elm_builtin Black yk yv (balance Black xk xv (redden a) b) (RBNode_elm_builtin Black zk zv c d)

                        _ ->
                            t

                _ ->
                    t
        else
            t



-- make the top node black


blacken : AllDict k v -> AllDict k v
blacken t =
    case t of
        RBEmpty_elm_builtin _ ->
            RBEmpty_elm_builtin LBlack

        RBNode_elm_builtin _ k v l r ->
            RBNode_elm_builtin Black k v l r



-- make the top node red


redden : AllDict k v -> AllDict k v
redden t =
    case t of
        RBEmpty_elm_builtin _ ->
            Debug.crash "can't make a Leaf red"

        RBNode_elm_builtin _ k v l r ->
            RBNode_elm_builtin Red k v l r


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> AllDict k a -> AllDict k b
map f dict =
    case dict of
        RBEmpty_elm_builtin clr ->
            RBEmpty_elm_builtin clr

        RBNode_elm_builtin clr key value left right ->
            RBNode_elm_builtin clr key (f key value) (map f left) (map f right)


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (k -> v -> b -> b) -> b -> AllDict k v -> b
foldl f acc dict =
    case dict of
        RBEmpty_elm_builtin _ ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldl f (f key value (foldl f acc left)) right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (k -> v -> b -> b) -> b -> AllDict k v -> b
foldr f acc t =
    case t of
        RBEmpty_elm_builtin _ ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldr f (f key value (foldr f acc right)) left


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Comparable k -> AllDict k v -> AllDict k v -> AllDict k v
union comparable t1 t2 =
    foldl (insert comparable) t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Comparable k -> AllDict k v -> AllDict k v -> AllDict k v
intersect comparable t1 t2 =
    filter comparable (\k _ -> member comparable k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Comparable k -> AllDict k v -> AllDict k v -> AllDict k v
diff comparable t1 t2 =
    foldl (\k v t -> remove comparable k t) t1 t2


{-| Get all of the keys in a dictionary.
-}
keys : AllDict k v -> List k
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary.
-}
values : AllDict k v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs.
-}
toList : AllDict k v -> List ( k, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
Takes a default value, and a list of key-pair tuples
-}
fromList : Comparable k -> List ( k, v ) -> AllDict k v
fromList comparable assocs =
    List.foldl (\( key, value ) dict -> insert comparable key value dict) empty assocs


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : Comparable k -> (k -> v -> Bool) -> AllDict k v -> AllDict k v
filter comparable predicate dictionary =
    let
        add key value dict =
            if predicate key value then
                insert comparable key value dict
            else
                dict
    in
        foldl add empty dictionary


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : Comparable k -> (k -> v -> Bool) -> AllDict k v -> ( AllDict k v, AllDict k v )
partition comparable predicate dict =
    let
        add key value ( t1, t2 ) =
            if predicate key value then
                ( insert comparable key value t1, t2 )
            else
                ( t1, insert comparable key value t2 )
    in
        foldl add ( empty, empty ) dict
