module Wizard.Documents.Index.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import List.Extra as List
import Shared.Data.Document exposing (Document)
import Shared.Data.Document.DocumentState exposing (DocumentState(..))
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.Submission exposing (Submission)
import Shared.Data.SubmissionService exposing (SubmissionService)
import Uuid exposing (Uuid)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { documents : Listing.Model Document
    , documentToBeDeleted : Maybe Document
    , deletingDocument : ActionResult String
    , questionnaireUuid : Maybe Uuid
    , questionnaire : Maybe (ActionResult QuestionnaireDetail)
    , documentToBeSubmitted : Maybe Document
    , submittingDocument : ActionResult Submission
    , submissionServices : ActionResult (List SubmissionService)
    , selectedSubmissionServiceId : Maybe String
    }


initialModel : Maybe Uuid -> PaginationQueryString -> Model
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
