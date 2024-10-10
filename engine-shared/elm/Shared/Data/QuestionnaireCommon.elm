module Shared.Data.QuestionnaireCommon exposing
    ( QuestionnaireCommon
    , decoder
    , updateWithQuestionnaireData
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Permission as Permission exposing (Permission)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Shared.Data.WebSockets.QuestionnaireAction.SetQuestionnaireData exposing (SetQuestionnaireData)
import Uuid exposing (Uuid)


type alias QuestionnaireCommon =
    { uuid : Uuid
    , name : String
    , isTemplate : Bool
    , permissions : List Permission
    , sharing : QuestionnaireSharing
    , visibility : QuestionnaireVisibility
    , migrationUuid : Maybe Uuid
    , packageId : String
    , fileCount : Int
    }


decoder : Decoder QuestionnaireCommon
decoder =
    D.succeed QuestionnaireCommon
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "isTemplate" D.bool
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "sharing" QuestionnaireSharing.decoder
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "migrationUuid" (D.nullable Uuid.decoder)
        |> D.required "packageId" D.string
        |> D.required "fileCount" D.int


updateWithQuestionnaireData : SetQuestionnaireData -> QuestionnaireCommon -> QuestionnaireCommon
updateWithQuestionnaireData data questionnaire =
    { questionnaire
        | name = data.name
        , isTemplate = data.isTemplate
        , permissions = data.permissions
        , sharing = data.sharing
        , visibility = data.visibility
    }
