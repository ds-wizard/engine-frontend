module Wizard.Api.Models.QuestionnaireSuggestion exposing
    ( QuestionnaireSuggestion
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias QuestionnaireSuggestion =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    }


decoder : Decoder QuestionnaireSuggestion
decoder =
    D.succeed QuestionnaireSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
