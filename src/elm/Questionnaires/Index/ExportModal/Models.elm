module Questionnaires.Index.ExportModal.Models exposing
    ( Model
    , Template
    , initialModel
    , setQuestionnaire
    , templateDecoder
    , templateListDecoder
    )

import ActionResult exposing (ActionResult(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Questionnaires.Common.Questionnaire exposing (Questionnaire)


type alias Model =
    { questionnaire : Maybe Questionnaire
    , templates : ActionResult (List Template)
    , selectedFormat : String
    , selectedTemplate : Maybe String
    }


initialModel : Model
initialModel =
    { questionnaire = Nothing
    , templates = Unset
    , selectedFormat = "pdf"
    , selectedTemplate = Nothing
    }


setQuestionnaire : Questionnaire -> Model -> Model
setQuestionnaire questionnaire model =
    { model
        | questionnaire = Just questionnaire
        , templates = Loading
    }


type alias Template =
    { uuid : String
    , name : String
    }


templateDecoder : Decoder Template
templateDecoder =
    Decode.succeed Template
        |> required "uuid" Decode.string
        |> required "name" Decode.string


templateListDecoder : Decoder (List Template)
templateListDecoder =
    Decode.list templateDecoder
