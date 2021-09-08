module Shared.Data.Document exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import List.Extra as List
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Data.Document.DocumentState as DocumentState exposing (DocumentState)
import Shared.Data.Document.DocumentTemplate as DocumentTemplate exposing (DocumentTemplate)
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireInfo as QuestionnaireInfo exposing (QuestionnaireInfo)
import Shared.Data.Template.TemplateFormat exposing (TemplateFormat)
import Shared.Data.UserInfo as UserInfo
import Time
import Uuid exposing (Uuid)


type alias Document =
    { uuid : Uuid
    , name : String
    , createdAt : Time.Posix
    , questionnaire : Maybe QuestionnaireInfo
    , questionnaireEventUuid : Maybe Uuid
    , template : DocumentTemplate
    , formatUuid : Uuid
    , state : DocumentState
    , creatorUuid : Maybe Uuid
    }


isOwner : AbstractAppState a -> Document -> Bool
isOwner appState document =
    appState.session.user
        |> Maybe.map (.uuid >> Just >> (==) document.creatorUuid)
        |> Maybe.withDefault False


decoder : Decoder Document
decoder =
    D.succeed Document
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "createdAt" D.datetime
        |> D.optional "questionnaire" (D.maybe QuestionnaireInfo.decoder) Nothing
        |> D.required "questionnaireEventUuid" (D.maybe Uuid.decoder)
        |> D.required "template" DocumentTemplate.decoder
        |> D.required "formatUuid" Uuid.decoder
        |> D.required "state" DocumentState.decoder
        |> D.required "creatorUuid" (D.maybe Uuid.decoder)


compare : Document -> Document -> Order
compare d1 d2 =
    Basics.compare (String.toLower d1.name) (String.toLower d2.name)


getFormat : Document -> Maybe TemplateFormat
getFormat document =
    List.find (.uuid >> (==) document.formatUuid) document.template.formats
