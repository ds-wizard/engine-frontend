module Common.Api.Models.KnowledgeModelPackageSuggestion exposing
    ( KnowledgeModelPackageSuggestion
    , decoder
    , encode
    , fromKnowledgeModelPackage
    , isSameKnowledgeModelPackage
    , knowledgeModelPackageIdAll
    )

import Common.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias KnowledgeModelPackageSuggestion =
    { uuid : Uuid
    , name : String
    , description : String
    , id : String
    , version : Version
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder KnowledgeModelPackageSuggestion
decoder =
    D.succeed KnowledgeModelPackageSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "id" D.string
        |> D.required "version" Version.decoder
        |> D.optional "workspaceUuid" (D.nullable Uuid.decoder) Nothing


encode : KnowledgeModelPackageSuggestion -> E.Value
encode kmPackageSuggestion =
    E.object
        [ ( "uuid", Uuid.encode kmPackageSuggestion.uuid )
        , ( "name", E.string kmPackageSuggestion.name )
        , ( "description", E.string kmPackageSuggestion.description )
        , ( "id", E.string kmPackageSuggestion.id )
        , ( "version", Version.encode kmPackageSuggestion.version )
        , ( "workspaceUuid", Maybe.unwrap E.null Uuid.encode kmPackageSuggestion.workspaceUuid )
        ]


fromKnowledgeModelPackage : KnowledgeModelPackage -> KnowledgeModelPackageSuggestion
fromKnowledgeModelPackage kmPackage =
    { uuid = kmPackage.uuid
    , name = kmPackage.name
    , description = kmPackage.description
    , id = kmPackage.id
    , version = kmPackage.version
    , workspaceUuid = kmPackage.workspaceUuid
    }


{-| Whether two package ids, each an id with a version or `all` after it, are
of the same knowledge model.
-}
isSameKnowledgeModelPackage : String -> String -> Bool
isSameKnowledgeModelPackage kmPackageId1 kmPackageId2 =
    let
        mbId1 =
            getKnowledgeModelPackageId kmPackageId1
    in
    Maybe.isJust mbId1 && mbId1 == getKnowledgeModelPackageId kmPackageId2


{-| Every version of a knowledge model, as a filter names it.
-}
knowledgeModelPackageIdAll : String -> String
knowledgeModelPackageIdAll id =
    id ++ ":all"


getKnowledgeModelPackageId : String -> Maybe String
getKnowledgeModelPackageId kmPackageId =
    case String.split ":" kmPackageId of
        id :: _ :: _ ->
            Just id

        _ ->
            Nothing
