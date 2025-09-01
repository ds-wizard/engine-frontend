module Wizard.Api.Models.QuestionnaireFile exposing
    ( QuestionnaireFile
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireInfo as QuestionnaireInfo exposing (QuestionnaireInfo)
import Wizard.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)


type alias QuestionnaireFile =
    { uuid : Uuid
    , contentType : String
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    , fileName : String
    , fileSize : Int
    , questionnaire : QuestionnaireInfo
    }


decoder : Decoder QuestionnaireFile
decoder =
    D.succeed QuestionnaireFile
        |> D.required "uuid" Uuid.decoder
        |> D.required "contentType" D.string
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.required "fileName" D.string
        |> D.required "fileSize" D.int
        |> D.required "questionnaire" QuestionnaireInfo.decoder
