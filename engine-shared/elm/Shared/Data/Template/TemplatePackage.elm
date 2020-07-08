module Shared.Data.Template.TemplatePackage exposing
    ( TemplatePackage
    , compare
    , decoder
    , toFormRichOption
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


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


matchId : Maybe String -> TemplatePackage -> Bool
matchId mbStringId =
    (==) mbStringId << Just << .id
