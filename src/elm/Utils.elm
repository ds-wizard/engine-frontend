module Utils exposing
    ( boolToInt
    , dispatch
    , getContrastColorHex
    , getUuid
    , listFilterJust
    , packageIdToComponents
    , pair
    , replace
    , splitVersion
    , stringToInt
    , tuplePrepend
    , validateRegex
    , versionIsGreater
    )

import Color
import Color.Accessibility exposing (contrastRatio)
import Color.Convert exposing (hexToColor)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Validate as Validate exposing (..)
import List.Extra as List
import Random exposing (Seed, step)
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
            (\s -> Validate.format (createRegex regex) s |> mapError (\_ -> Error.value InvalidFormat))


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
            String.split "." version |> List.map (String.toInt >> Maybe.withDefault 0)
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
stringToInt =
    String.toInt >> Maybe.withDefault 0


createRegex : String -> Regex
createRegex regex =
    Maybe.withDefault Regex.never <| Regex.fromString regex


boolToInt : Bool -> Int
boolToInt bool =
    if bool then
        1

    else
        0


listFilterJust : List (Maybe a) -> List a
listFilterJust =
    let
        fold item currentList =
            case item of
                Just value ->
                    value :: currentList

                Nothing ->
                    currentList
    in
    List.foldl fold []


getContrastColorHex : String -> String
getContrastColorHex colorHex =
    case hexToColor colorHex of
        Ok color ->
            let
                blackContrast =
                    contrastRatio Color.black color

                whiteContrast =
                    contrastRatio Color.white color
            in
            if blackContrast > whiteContrast then
                "#000000"

            else
                "#ffffff"

        _ ->
            "#000000"


packageIdToComponents : String -> Maybe ( String, String, String )
packageIdToComponents packageId =
    case String.split ":" packageId of
        orgId :: kmId :: version :: [] ->
            Just ( orgId, kmId, version )

        _ ->
            Nothing
