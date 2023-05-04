module Shared.Utils exposing
    ( boolToInt
    , boolToString
    , compose2
    , dictFromMaybeList
    , dispatch
    , flip
    , getContrastColorHex
    , getOrganizationAndItemId
    , getUuid
    , getUuidString
    , httpMethodOptions
    , listFilterJust
    , listInsertIf
    , nilUuid
    , packageIdToComponents
    , tuplePrepend
    , withNoCmd
    )

import Color
import Color.Convert exposing (hexToColor)
import Dict exposing (Dict)
import List.Extra as List
import Random exposing (Seed, step)
import Task
import Uuid exposing (Uuid)


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


compose2 : (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 g f x y =
    g (f x y)


tuplePrepend : a -> ( b, c ) -> ( a, b, c )
tuplePrepend a ( b, c ) =
    ( a, b, c )


getUuidString : Seed -> ( String, Seed )
getUuidString =
    Tuple.mapFirst Uuid.toString << getUuid


getUuid : Seed -> ( Uuid, Seed )
getUuid =
    step Uuid.uuidGenerator


nilUuid : String
nilUuid =
    "00000000-0000-0000-0000-000000000000"


httpMethodOptions : List ( String, String )
httpMethodOptions =
    let
        httpMethods =
            [ "GET", "POST", "HEAD", "PUT", "DELETE", "OPTIONS", "PATCH" ]
    in
    ( "", "--" ) :: List.zip httpMethods httpMethods


dispatch : a -> Cmd a
dispatch msg =
    Task.perform (always msg) (Task.succeed ())


boolToInt : Bool -> Int
boolToInt bool =
    if bool then
        1

    else
        0


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"


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


dictFromMaybeList : List ( comparable, Maybe a ) -> Dict comparable a
dictFromMaybeList list =
    let
        fold ( key, mbItem ) acc =
            case mbItem of
                Just item ->
                    Dict.insert key item acc

                Nothing ->
                    acc
    in
    List.foldl fold Dict.empty list


getContrastColorHex : String -> String
getContrastColorHex colorHex =
    case hexToColor colorHex of
        Ok color ->
            let
                rgba =
                    Color.toRgba color

                redValue =
                    255 * 0.299 * rgba.red

                blueValue =
                    255 * 0.587 * rgba.blue

                greenValue =
                    255 * 0.114 * rgba.green
            in
            if redValue + blueValue + greenValue > 186 then
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


getOrganizationAndItemId : String -> String
getOrganizationAndItemId fullId =
    case String.split ":" fullId of
        organizationId :: itemId :: _ ->
            organizationId ++ ":" ++ itemId

        _ ->
            ""


withNoCmd : model -> ( model, Cmd msg )
withNoCmd model =
    ( model, Cmd.none )
