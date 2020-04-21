module Shared.Data.Template exposing
    ( Template
    , decoder
    , findByUuid
    , toFormRichOption
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Shared.Data.Template.TemplatePacakge as TemplatePackage exposing (TemplatePackage)
import Uuid exposing (Uuid)
import WizardResearch.Common.AppState exposing (AppState)


type alias Template =
    { uuid : Uuid
    , name : String
    , description : String
    , recommendedPackageId : String
    , allowedPackages : List TemplatePackage
    , formats : List TemplateFormat
    }


decoder : Decoder Template
decoder =
    D.succeed Template
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "recommendedPackageId" D.string
        |> D.required "allowedPackages" (D.list TemplatePackage.decoder)
        |> D.required "formats" (D.list TemplateFormat.decoder)


toFormRichOption : AppState -> Template -> ( String, String, String )
toFormRichOption appState { uuid, name, description } =
    let
        stringUuid =
            Uuid.toString uuid

        visibleName =
            if Just stringUuid == appState.config.template.recommendedTemplateUuid then
                name ++ " (recommended)"

            else
                name
    in
    ( stringUuid, visibleName, description )


findByUuid : List Template -> String -> Maybe Template
findByUuid templates templateUuid =
    List.find (.uuid >> Uuid.toString >> (==) templateUuid) templates
