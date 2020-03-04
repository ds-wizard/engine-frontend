module Wizard.Documents.Index.Msgs exposing (..)

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing as Listing
import Wizard.Documents.Common.Document exposing (Document)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type Msg
    = GetDocumentsCompleted (Result ApiError (List Document))
    | GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | ShowHideDeleteDocument (Maybe Document)
    | DeleteDocument
    | DeleteDocumentCompleted (Result ApiError ())
    | ListingMsg Listing.Msg
    | RefreshDocuments
    | RefreshDocumentsCompleted (Result ApiError (List Document))
