module Wizard.Projects.Detail.Documents.Msgs exposing (Msg(..))

import Shared.Data.Document exposing (Document)
import Shared.Data.Submission exposing (Submission)
import Shared.Data.SubmissionService exposing (SubmissionService)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteDocument (Maybe Document)
    | DeleteDocument
    | DeleteDocumentCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Document)
    | ShowHideSubmitDocument (Maybe Document)
    | GetSubmissionServicesCompleted (Result ApiError (List SubmissionService))
    | SelectSubmissionService String
    | SubmitDocument
    | SubmitDocumentCompleted (Result ApiError Submission)
    | SetDocumentErrorModal (Maybe String)
    | SetSubmissionErrorModal (Maybe String)
    | DownloadDocument Document
