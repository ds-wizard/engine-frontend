module Wizard.Api.Models.QuestionnaireCommon exposing
    ( QuestionnaireCommon
    , decoder
    , updateWithQuestionnaireData
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.Permission as Permission exposing (Permission)
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Wizard.Api.Models.WebSockets.QuestionnaireAction.SetQuestionnaireData exposing (SetQuestionnaireData)


type alias QuestionnaireCommon =
    { uuid : Uuid
    , name : String
    , isTemplate : Bool
    , permissions : List Permission
    , sharing : QuestionnaireSharing
    , visibility : QuestionnaireVisibility
    , migrationUuid : Maybe Uuid
    , knowledgeModelPackageId : String
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
        |> D.required "knowledgeModelPackageId" D.string
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
