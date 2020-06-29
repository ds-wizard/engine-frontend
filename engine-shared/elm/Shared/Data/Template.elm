module Shared.Data.Template exposing
    ( Template
    , compare
    , decoder
    , findById
    , toFormRichOption
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import List.Extra as List
import Shared.Data.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Shared.Data.Template.TemplatePackage as TemplatePackage exposing (TemplatePackage)
import Shared.Data.Template.TemplateState as TemplateState exposing (TemplateState)
import Time
import Version exposing (Version)


type alias Template =
    { createdAt : Time.Posix
    , description : String
    , formats : List TemplateFormat
    , id : String
    , license : String
    , metamodelVersion : Int
    , name : String
    , organization : Maybe OrganizationInfo
    , organizationId : String
    , readme : String
    , recommendedPackageId : Maybe String
    , state : TemplateState
    , templateId : String
    , usablePackages : List TemplatePackage
    , version : Version
    }


decoder : Decoder Template
decoder =
    D.succeed Template
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "formats" (D.list TemplateFormat.decoder)
        |> D.required "id" D.string
        |> D.required "license" D.string
        |> D.required "metamodelVersion" D.int
        |> D.required "name" D.string
        |> D.optional "organization" (D.maybe OrganizationInfo.decoder) Nothing
        |> D.required "organizationId" D.string
        |> D.required "readme" D.string
        |> D.required "recommendedPackageId" (D.maybe D.string)
        |> D.required "state" TemplateState.decoder
        |> D.required "templateId" D.string
        |> D.required "usablePackages" (D.list TemplatePackage.decoder)
        |> D.required "version" Version.decoder


toFormRichOption : Maybe String -> Template -> ( String, String, String )
toFormRichOption recommendedTemplateId template =
    let
        visibleName =
            if matchId recommendedTemplateId template then
                template.name ++ " (recommended)"

            else
                template.name
    in
    ( template.id, visibleName, template.description )


compare : Maybe String -> Template -> Template -> Order
compare recommendedTemplateId t1 t2 =
    if matchId recommendedTemplateId t1 then
        LT

    else if matchId recommendedTemplateId t2 then
        GT

    else
        Basics.compare t1.name t2.name


matchId : Maybe String -> Template -> Bool
matchId mbId =
    (==) mbId << Just << .id


findById : List Template -> String -> Maybe Template
findById templates templateId =
    List.find (.id >> (==) templateId) templates
