module Wizard.Documents.Common.Document exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Common.DocumentState as DocumentState exposing (DocumentState)
import Wizard.Questionnaires.Common.Questionnaire as Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.Template as Template exposing (Template)
import Wizard.Users.Common.User as User


type alias Document =
    { uuid : String
    , name : String
    , createdAt : Time.Posix
    , questionnaire : Maybe Questionnaire
    , template : Template
    , format : String
    , state : DocumentState
    , ownerUuid : String
    }


isEditable : AppState -> Document -> Bool
isEditable appState document =
    let
        isAdmin =
            User.isAdmin appState.session.user

        isOwner =
            appState.session.user
                |> Maybe.map (.uuid >> (==) document.ownerUuid)
                |> Maybe.withDefault False
    in
    isAdmin || isOwner


decoder : Decoder Document
decoder =
    D.succeed Document
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "createdAt" D.datetime
        |> D.optional "questionnaire" (D.maybe Questionnaire.decoder) Nothing
        |> D.required "template" Template.decoder
        |> D.required "format" D.string
        |> D.required "state" DocumentState.decoder
        |> D.required "ownerUuid" D.string


compare : Document -> Document -> Order
compare d1 d2 =
    Basics.compare (String.toLower d1.name) (String.toLower d2.name)
