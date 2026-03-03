module Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing
    ( KnowledgeModelPackageSuggestion
    , decoder
    , encode
    , fromKnowledgeModelPackage
    , isSameKnowledgeModelPackage
    , knowledgeModelPackageIdAll
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)


type alias KnowledgeModelPackageSuggestion =
    { uuid : Uuid
    , name : String
    , description : String
    , organizationId : String
    , kmId : String
    , version : Version
    }


decoder : Decoder KnowledgeModelPackageSuggestion
decoder =
    D.succeed KnowledgeModelPackageSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "organizationId" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder


encode : KnowledgeModelPackageSuggestion -> E.Value
encode kmPackageSuggestion =
    E.object
        [ ( "uuid", Uuid.encode kmPackageSuggestion.uuid )
        , ( "name", E.string kmPackageSuggestion.name )
        , ( "description", E.string kmPackageSuggestion.description )
        , ( "organizationId", E.string kmPackageSuggestion.organizationId )
        , ( "kmId", E.string kmPackageSuggestion.kmId )
        , ( "version", Version.encode kmPackageSuggestion.version )
        ]


fromKnowledgeModelPackage : KnowledgeModelPackage -> KnowledgeModelPackageSuggestion
fromKnowledgeModelPackage kmPackage =
    { uuid = kmPackage.uuid
    , name = kmPackage.name
    , description = kmPackage.description
    , organizationId = kmPackage.organizationId
    , kmId = kmPackage.kmId
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


knowledgeModelPackageIdAll : String -> String -> String
knowledgeModelPackageIdAll orgId kmId =
    orgId ++ ":" ++ kmId ++ ":all"


getKnowledgeModelPackageIdValues : String -> ( Maybe String, Maybe String )
getKnowledgeModelPackageIdValues kmPackageId =
    case String.split ":" kmPackageId of
        orgId :: kmId :: _ ->
            ( Just orgId, Just kmId )

        _ ->
            ( Nothing, Nothing )
