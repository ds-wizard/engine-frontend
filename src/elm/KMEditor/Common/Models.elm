module KMEditor.Common.Models exposing
    ( KnowledgeModel
    , KnowledgeModelDetail
    , KnowledgeModelState(..)
    , kmLastVersion
    , kmMatchState
    , knowledgeModelDecoder
    , knowledgeModelDetailDecoder
    , knowledgeModelListDecoder
    , knowledgeModelStateDecoder
    )

import Json.Decode as D exposing (..)
import Json.Decode.Extra as D
import Json.Decode.Pipeline exposing (optional, required)
import KMEditor.Common.Models.Events exposing (Event, eventDecoder)
import KnowledgeModels.Common.Version as Version exposing (Version)
import List.Extra as List
import Time


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , organizationId : String
    , kmId : String
    , parentPackageId : Maybe String
    , lastAppliedParentPackageId : Maybe String
    , stateType : KnowledgeModelState
    , updatedAt : Time.Posix
    }


type alias KnowledgeModelDetail =
    { uuid : String
    , name : String
    , kmId : String
    , organizationId : String
    , parentPackageId : Maybe String
    , events : List Event
    }


type KnowledgeModelState
    = Default
    | Edited
    | Outdated
    | Migrating
    | Migrated


knowledgeModelDecoder : Decoder KnowledgeModel
knowledgeModelDecoder =
    D.succeed KnowledgeModel
        |> required "uuid" D.string
        |> required "name" D.string
        |> required "organizationId" D.string
        |> required "kmId" D.string
        |> required "parentPackageId" (D.nullable D.string)
        |> required "lastAppliedParentPackageId" (D.nullable D.string)
        |> optional "stateType" knowledgeModelStateDecoder Default
        |> required "updatedAt" D.datetime


knowledgeModelStateDecoder : Decoder KnowledgeModelState
knowledgeModelStateDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "Default" ->
                        D.succeed Default

                    "Edited" ->
                        D.succeed Edited

                    "Outdated" ->
                        D.succeed Outdated

                    "Migrating" ->
                        D.succeed Migrating

                    "Migrated" ->
                        D.succeed Migrated

                    unknownState ->
                        D.fail <| "Unknown knowledge model appState " ++ unknownState
            )


knowledgeModelListDecoder : Decoder (List KnowledgeModel)
knowledgeModelListDecoder =
    D.list knowledgeModelDecoder


knowledgeModelDetailDecoder : Decoder KnowledgeModelDetail
knowledgeModelDetailDecoder =
    D.succeed KnowledgeModelDetail
        |> required "uuid" D.string
        |> required "name" D.string
        |> required "kmId" D.string
        |> required "organizationId" D.string
        |> required "parentPackageId" (D.nullable D.string)
        |> required "events" (D.list eventDecoder)


kmMatchState : List KnowledgeModelState -> KnowledgeModel -> Bool
kmMatchState states knowledgeModel =
    List.any ((==) knowledgeModel.stateType) states


kmLastVersion : KnowledgeModelDetail -> Maybe Version
kmLastVersion km =
    let
        getVersion parent =
            let
                parts =
                    String.split ":" parent

                samePackage =
                    List.getAt 1 parts
                        |> Maybe.map ((==) km.kmId)
                        |> Maybe.withDefault False

                sameOrganization =
                    List.getAt 0 parts
                        |> Maybe.map ((==) km.organizationId)
                        |> Maybe.withDefault False
            in
            if sameOrganization && samePackage then
                List.getAt 2 parts
                    |> Maybe.andThen Version.fromString

            else
                Nothing
    in
    km.parentPackageId
        |> Maybe.andThen getVersion
