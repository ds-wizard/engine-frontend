module Shared.Data.Template.TemplatePackage exposing
    ( TemplatePackage
    , compare
    , compareById
    , decoder
    , toFormRichOption
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Version


type alias TemplatePackage =
    { id : String
    , name : String
    , description : String
    }


decoder : Decoder TemplatePackage
decoder =
    D.succeed TemplatePackage
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string


toFormRichOption : Maybe String -> TemplatePackage -> ( String, String, String )
toFormRichOption mbRecommendedId tp =
    let
        visibleName =
            if matchId mbRecommendedId tp then
                tp.name ++ " (recommended)"

            else
                tp.name
    in
    ( tp.id, visibleName, tp.description )


compare : Maybe String -> TemplatePackage -> TemplatePackage -> Order
compare mbRecommendedId tp1 tp2 =
    if matchId mbRecommendedId tp1 then
        LT

    else if matchId mbRecommendedId tp2 then
        GT

    else
        Basics.compare tp1.name tp2.name


compareById : TemplatePackage -> TemplatePackage -> Order
compareById tp1 tp2 =
    let
        split tp =
            case String.split ":" tp of
                orgId :: pkgId :: version :: [] ->
                    ( orgId, pkgId, version )

                _ ->
                    ( "", "", "" )

        ( tp1orgId, tp1PkgId, tp1version ) =
            split tp1.id

        ( tp2orgId, tp2PkgId, tp2version ) =
            split tp2.id
    in
    if tp1orgId /= tp2orgId then
        Basics.compare tp1orgId tp2orgId

    else if tp1PkgId /= tp2PkgId then
        Basics.compare tp1PkgId tp2PkgId

    else
        case ( Version.fromString tp1version, Version.fromString tp2version ) of
            ( Just version1, Just version2 ) ->
                Version.compare version1 version2

            _ ->
                EQ


matchId : Maybe String -> TemplatePackage -> Bool
matchId mbStringId =
    (==) mbStringId << Just << .id
