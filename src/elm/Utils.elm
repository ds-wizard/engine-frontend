module Utils exposing
    ( boolToInt
    , dispatch
    , flip
    , getContrastColorHex
    , getUuid
    , listFilterJust
    , listInsertIf
    , nilUuid
    , packageIdToComponents
    , pair
    , stringToInt
    , tuplePrepend
    , validateRegex
    , withNoCmd
    )

import Color
import Color.Accessibility exposing (contrastRatio)
import Color.Convert exposing (hexToColor)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Validate as Validate exposing (..)
import Random exposing (Seed, step)
import Regex exposing (Regex)
import Task
import Uuid


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


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


nilUuid : String
nilUuid =
    "00000000-0000-0000-0000-000000000000"


dispatch : a -> Cmd a
dispatch msg =
    Task.perform (always msg) (Task.succeed ())


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
                    currentList ++ [ value ]

                Nothing ->
                    currentList
    in
    List.foldl fold []


listInsertIf : a -> Bool -> List a -> List a
listInsertIf item shouldBeInserted list =
    if shouldBeInserted then
        list ++ [ item ]

    else
        list


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


withNoCmd : model -> ( model, Cmd msg )
withNoCmd model =
    ( model, Cmd.none )
