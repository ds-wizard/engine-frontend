module Shared.Data.Template.TemplatePacakge exposing
    ( TemplatePackage
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
toFormRichOption mbRecommendedId { id, name, description } =
    let
        visibleName =
            if mbRecommendedId == Just id then
                name ++ " (recommended)"

            else
                name
    in
    ( id, visibleName, description )
