module Wizard.Documents.Index.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import List.Extra as List
import Wizard.Common.Components.Listing as Listing
import Wizard.Documents.Common.Document exposing (Document)
import Wizard.Documents.Common.DocumentState exposing (DocumentState(..))
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type alias Model =
    { documents : ActionResult (Listing.Model Document)
    , documentToBeDeleted : Maybe Document
    , deletingDocument : ActionResult String
    , questionnaireUuid : Maybe String
    , questionnaire : Maybe (ActionResult QuestionnaireDetail)
    }


initialModel : Maybe String -> Model
initialModel questionnaireUuid =
    { documents = Loading
    , documentToBeDeleted = Nothing
    , deletingDocument = Unset
    , questionnaireUuid = questionnaireUuid
    , questionnaire = Maybe.map (\_ -> Loading) questionnaireUuid
    }


updateStates : Model -> List Document -> Model
updateStates model documents =
    let
        setNewState item =
            let
                newState =
                    documents
                        |> List.find (\d -> d.uuid == item.uuid)
                        |> Maybe.map .state
                        |> Maybe.withDefault item.state
            in
            { item | state = newState }

        transformItems listingModel =
            let
                newItems =
                    listingModel.items
                        |> List.map (\item -> { item | item = setNewState item.item })
            in
            { listingModel | items = newItems }
    in
    { model | documents = ActionResult.map transformItems model.documents }


anyDocumentInProgress : Model -> Bool
anyDocumentInProgress model =
    let
        isInProgress item =
            item.item.state == QueuedDocumentState || item.item.state == InProgressDocumentState
    in
    model.documents
        |> ActionResult.map .items
        |> ActionResult.withDefault []
        |> List.any isInProgress
