module Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing
    ( KnowledgeModelPackageSuggestion
    , decoder
    , fromKnowledgeModelPackage
    , isSameKnowledgeModelPackage
    , knowledgeModelPackageIdAll
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Version exposing (Version)
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)


type alias KnowledgeModelPackageSuggestion =
    { id : String
    , name : String
    , description : String
    , version : Version
    }


decoder : Decoder KnowledgeModelPackageSuggestion
decoder =
    D.succeed KnowledgeModelPackageSuggestion
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "version" Version.decoder


fromKnowledgeModelPackage : KnowledgeModelPackage -> KnowledgeModelPackageSuggestion
fromKnowledgeModelPackage kmPackage =
    { id = kmPackage.id
    , name = kmPackage.name
    , description = kmPackage.description
    , version = kmPackage.version
    }


isSameKnowledgeModelPackage : String -> String -> Bool
isSameKnowledgeModelPackage kmPackageId1 kmPackageId2 =
    let
        ( mbOrgId1, mbKmId1 ) =
            getKnowledgeModelPackageIdValues kmPackageId1

        ( mbOrgId2, mbKmId2 ) =
            getKnowledgeModelPackageIdValues kmPackageId2
    in
    Maybe.isJust mbOrgId1 && mbOrgId1 == mbOrgId2 && Maybe.isJust mbKmId1 && mbKmId1 == mbKmId2


knowledgeModelPackageIdAll : String -> String
knowledgeModelPackageIdAll kmPackageId =
    case getKnowledgeModelPackageIdValues kmPackageId of
        ( Just orgId, Just kmId ) ->
            orgId ++ ":" ++ kmId ++ ":all"

        _ ->
            kmPackageId


getKnowledgeModelPackageIdValues : String -> ( Maybe String, Maybe String )
getKnowledgeModelPackageIdValues kmPackageId =
    case String.split ":" kmPackageId of
        orgId :: kmId :: _ ->
            ( Just orgId, Just kmId )

        _ ->
            ( Nothing, Nothing )
