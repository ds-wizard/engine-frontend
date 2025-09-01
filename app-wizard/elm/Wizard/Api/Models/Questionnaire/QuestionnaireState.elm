module Wizard.Api.Models.Questionnaire.QuestionnaireState exposing
    ( QuestionnaireState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type QuestionnaireState
    = Default
    | Outdated
    | Migrating


decoder : Decoder QuestionnaireState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "Default" ->
                        D.succeed Default

                    "Outdated" ->
                        D.succeed Outdated

                    "Migrating" ->
                        D.succeed Migrating

                    unknownState ->
                        D.fail <| "Unknown questionnaire state " ++ unknownState
            )
