module Wizard.Documents.Index.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import List.Extra as List
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Common.Pagination.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Documents.Common.Document exposing (Document)
import Wizard.Documents.Common.DocumentState exposing (DocumentState(..))
import Wizard.Documents.Common.Submission exposing (Submission)
import Wizard.Documents.Common.SubmissionService exposing (SubmissionService)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type alias Model =
    { documents : Listing.Model Document
    , documentToBeDeleted : Maybe Document
    , deletingDocument : ActionResult String
    , questionnaireUuid : Maybe String
    , questionnaire : Maybe (ActionResult QuestionnaireDetail)
    , documentToBeSubmitted : Maybe Document
    , submittingDocument : ActionResult Submission
    , submissionServices : ActionResult (List SubmissionService)
    , selectedSubmissionServiceId : Maybe String
    }


initialModel : Maybe String -> PaginationQueryString -> Model
initialModel questionnaireUuid paginationQueryString =
    { documents = Listing.initialModel paginationQueryString
    , documentToBeDeleted = Nothing
    , deletingDocument = Unset
    , questionnaireUuid = questionnaireUuid
    , questionnaire = Maybe.map (\_ -> Loading) questionnaireUuid
    , documentToBeSubmitted = Nothing
    , submittingDocument = Unset
    , submissionServices = Unset
    , selectedSubmissionServiceId = Nothing
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
    { model | documents = transformItems model.documents }


anyDocumentInProgress : Model -> Bool
anyDocumentInProgress model =
    let
        isInProgress document =
            document.state == QueuedDocumentState || document.state == InProgressDocumentState
    in
    model.documents.pagination
        |> ActionResult.map .items
        |> ActionResult.withDefault []
        |> List.any isInProgress
