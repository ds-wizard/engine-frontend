module Wizard.Pages.KMEditor.Publish.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Form
import Version exposing (Version)
import Wizard.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)


type Msg
    = GetKnowledgeModelEditorCompleted (Result ApiError KnowledgeModelEditorDetail)
    | GetPreviousKnowledgeModelPackageCompleted (Result ApiError KnowledgeModelPackageDetail)
    | Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PutKnowledgeModelEditorCompleted (Result ApiError KnowledgeModelPackage)
