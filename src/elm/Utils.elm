module Utils exposing (..)

import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Validate as Validate exposing (..)
import List.Extra as List
import Random.Pcg exposing (Seed, step)
import Regex exposing (Regex)
import Task
import Uuid


pair : a -> b -> ( a, b )
pair a b =
    ( a, b )


tuplePrepend : a -> ( b, c ) -> ( a, b, c )
tuplePrepend a ( b, c ) =
    ( a, b, c )


validateRegex : String -> Validation e String
validateRegex regex =
    Validate.string
        |> Validate.andThen
            (\s -> Validate.format (Regex.regex regex) s |> mapError (\_ -> Error.value InvalidFormat))


getUuid : Seed -> ( String, Seed )
getUuid seed =
    let
        ( uuid, newSeed ) =
            step Uuid.uuidGenerator seed
    in
    ( Uuid.toString uuid, newSeed )


versionIsGreater : String -> String -> Bool
versionIsGreater than version =
    case ( splitVersion version, splitVersion than ) of
        ( Just ( versionMajor, versionMinor, versionPatch ), Just ( thanMajor, thanMinor, thanPatch ) ) ->
            versionMajor > thanMajor || (versionMajor == thanMajor && (versionMinor > thanMinor || (versionMinor == thanMinor && versionPatch > thanPatch)))

        _ ->
            False


splitVersion : String -> Maybe ( Int, Int, Int )
splitVersion version =
    let
        parts =
            String.split "." version |> List.map (String.toInt >> Result.withDefault 0)
    in
    case ( List.getAt 0 parts, List.getAt 1 parts, List.getAt 2 parts ) of
        ( Just major, Just minor, Just patch ) ->
            Just ( major, minor, patch )

        _ ->
            Nothing


dispatch : a -> Cmd a
dispatch msg =
    Task.perform (always msg) (Task.succeed ())


replace : String -> String -> String -> String
replace from to str =
    String.split from str
        |> String.join to


stringToInt : String -> Int
stringToInt str =
    case String.toInt str of
        Ok value ->
            value

        Err _ ->
            0
