module Shared.Data.Template exposing
    ( Template
    , compare
    , decoder
    , findByUuid
    , toFormRichOption
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Shared.Data.Template.TemplatePackage as TemplatePackage exposing (TemplatePackage)
import Uuid exposing (Uuid)


type alias Template =
    { uuid : Uuid
    , name : String
    , description : String
    , recommendedPackageId : Maybe String
    , allowedPackages : List TemplatePackage
    , formats : List TemplateFormat
    }


decoder : Decoder Template
decoder =
    D.succeed Template
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "recommendedPackageId" (D.maybe D.string)
        |> D.required "allowedPackages" (D.list TemplatePackage.decoder)
        |> D.required "formats" (D.list TemplateFormat.decoder)


toFormRichOption : Maybe String -> Template -> ( String, String, String )
toFormRichOption recommendedTemplateUuid template =
    let
        visibleName =
            if matchUuid recommendedTemplateUuid template then
                template.name ++ " (recommended)"

            else
                template.name
    in
    ( Uuid.toString template.uuid, visibleName, template.description )


compare : Maybe String -> Template -> Template -> Order
compare recommendedTemplateUuid t1 t2 =
    if matchUuid recommendedTemplateUuid t1 then
        LT

    else if matchUuid recommendedTemplateUuid t2 then
        GT

    else
        Basics.compare t1.name t2.name


matchUuid : Maybe String -> Template -> Bool
matchUuid mbStringUuid =
    (==) mbStringUuid << Just << Uuid.toString << .uuid


findByUuid : List Template -> String -> Maybe Template
findByUuid templates templateUuid =
    List.find (.uuid >> Uuid.toString >> (==) templateUuid) templates
