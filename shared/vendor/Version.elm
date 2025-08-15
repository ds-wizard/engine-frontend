module Version exposing
    ( Version(..)
    , compare
    , create
    , decoder
    , encode
    , fromString
    , getMajor
    , getMinor
    , getPatch
    , greaterThan
    , nextMajor
    , nextMinor
    , nextPatch
    , toString
    , toStringMinor
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Maybe.Extra as Maybe


type Version
    = Version Int Int Int


create : Int -> Int -> Int -> Version
create =
    Version


getMajor : Version -> Int
getMajor (Version value _ _) =
    value


getMinor : Version -> Int
getMinor (Version _ value _) =
    value


getPatch : Version -> Int
getPatch (Version _ _ value) =
    value


nextMajor : Version -> Version
nextMajor (Version major _ _) =
    Version (major + 1) 0 0


nextMinor : Version -> Version
nextMinor (Version major minor _) =
    Version major (minor + 1) 0


nextPatch : Version -> Version
nextPatch (Version major minor patch) =
    Version major minor (patch + 1)


decoder : Decoder Version
decoder =
    D.string
        |> D.andThen
            (\str ->
                Maybe.unwrap (D.fail ("Invalid version " ++ str)) D.succeed (fromString str)
            )


encode : Version -> E.Value
encode =
    E.string << toString


toString : Version -> String
toString (Version major minor patch) =
    String.fromInt major ++ "." ++ String.fromInt minor ++ "." ++ String.fromInt patch


toStringMinor : Version -> String
toStringMinor (Version major minor _) =
    String.fromInt major ++ "." ++ String.fromInt minor


fromString : String -> Maybe Version
fromString versionString =
    case List.map String.toInt <| String.split "." versionString of
        (Just major) :: (Just minor) :: (Just patch) :: [] ->
            Just <| create major minor patch

        (Just major) :: (Just minor) :: [] ->
            Just <| create major minor 0

        (Just major) :: [] ->
            Just <| create major 0 0

        _ ->
            Nothing


greaterThan : Version -> Version -> Bool
greaterThan version1 version2 =
    compare version1 version2 == LT


compare : Version -> Version -> Order
compare (Version major1 minor1 patch1) (Version major2 minor2 patch2) =
    if major1 < major2 then
        LT

    else if major1 > major2 then
        GT

    else if minor1 < minor2 then
        LT

    else if minor1 > minor2 then
        GT

    else if patch1 < patch2 then
        LT

    else if patch1 > patch2 then
        GT

    else
        EQ
