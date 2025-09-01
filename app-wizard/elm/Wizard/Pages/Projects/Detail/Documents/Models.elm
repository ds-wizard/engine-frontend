module Wizard.Pages.Projects.Detail.Documents.Models exposing
    ( Model
    , addDocumentSubmission
    , anyDocumentInProgress
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.Document exposing (Document)
import Wizard.Api.Models.Document.DocumentState exposing (DocumentState(..))
import Wizard.Api.Models.Submission exposing (Submission)
import Wizard.Api.Models.SubmissionService exposing (SubmissionService)
import Wizard.Components.Listing.Models as Listing


type alias Model =
    { documents : Listing.Model Document
    , documentToBeDeleted : Maybe Document
    , deletingDocument : ActionResult String
    , documentToBeSubmitted : Maybe Document
    , submittingDocument : ActionResult Submission
    , submissionServices : ActionResult (List SubmissionService)
    , selectedSubmissionServiceId : Maybe String
    , documentErrorModal : Maybe String
    , submissionErrorModal : Maybe String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { documents = Listing.initialModel paginationQueryString
    , documentToBeDeleted = Nothing
    , deletingDocument = Unset
    , documentToBeSubmitted = Nothing
    , submittingDocument = Unset
    , submissionServices = Unset
    , selectedSubmissionServiceId = Nothing
    , documentErrorModal = Nothing
    , submissionErrorModal = Nothing
    }


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


addDocumentSubmission : Submission -> Model -> Model
addDocumentSubmission submission model =
    let
        updateItem document =
            if submission.documentUuid == document.uuid then
                { document | submissions = submission :: document.submissions }

            else
                document
    in
    { model | documents = Listing.updateItems updateItem model.documents }
