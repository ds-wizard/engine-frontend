module KMEditor.Common.Models exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import List.Extra as List


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , organizationId : String
    , kmId : String
    , parentPackageId : Maybe String
    , lastAppliedParentPackageId : Maybe String
    , stateType : KnowledgeModelState
    }


type KnowledgeModelState
    = Default
    | Edited
    | Outdated
    | Migrating
    | Migrated


knowledgeModelDecoder : Decoder KnowledgeModel
knowledgeModelDecoder =
    decode KnowledgeModel
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "organizationId" Decode.string
        |> required "kmId" Decode.string
        |> required "parentPackageId" (Decode.nullable Decode.string)
        |> required "lastAppliedParentPackageId" (Decode.nullable Decode.string)
        |> required "stateType" knowledgeModelStateDecoder


knowledgeModelStateDecoder : Decoder KnowledgeModelState
knowledgeModelStateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Default" ->
                        Decode.succeed Default

                    "Edited" ->
                        Decode.succeed Edited

                    "Outdated" ->
                        Decode.succeed Outdated

                    "Migrating" ->
                        Decode.succeed Migrating

                    "Migrated" ->
                        Decode.succeed Migrated

                    unknownState ->
                        Decode.fail <| "Unknown knowledge model state " ++ unknownState
            )


knowledgeModelListDecoder : Decoder (List KnowledgeModel)
knowledgeModelListDecoder =
    Decode.list knowledgeModelDecoder


kmMatchState : List KnowledgeModelState -> KnowledgeModel -> Bool
kmMatchState states knowledgeModel =
    List.any ((==) knowledgeModel.stateType) states


kmLastVersion : KnowledgeModel -> Maybe String
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
            else
                Nothing
    in
    km.parentPackageId
        |> Maybe.andThen getVersion
