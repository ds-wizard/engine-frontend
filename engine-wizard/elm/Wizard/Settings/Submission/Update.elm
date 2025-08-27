module Wizard.Settings.Submission.Update exposing
    ( fetchData
    , update
    )

import Gettext exposing (gettext)
import Shared.Utils.RequestHelpers as RequestHelpers
import Shared.Utils.Setters exposing (setTemplates)
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Api.Models.EditableConfig.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Submission.Models exposing (Model)
import Wizard.Settings.Submission.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ DocumentTemplatesApi.getTemplatesAll appState GetTemplatesCompleted
        , Cmd.map GenericMsg <| GenericUpdate.fetchData appState
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GenericMsg genericMsg ->
            let
                ( genericModel, cmd ) =
                    GenericUpdate.update updateProps (wrapMsg << GenericMsg) genericMsg appState model.genericModel
            in
            ( { model | genericModel = genericModel }, cmd )

        GetTemplatesCompleted result ->
            RequestHelpers.applyResult
                { setResult = setTemplates
                , defaultError = gettext "Unable to get document templates." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }


updateProps : GenericUpdate.UpdateProps EditableSubmissionConfig
updateProps =
    { initForm = .submission >> EditableSubmissionConfig.initForm
    , formToConfig = EditableConfig.updateSubmission
    , formValidation = EditableSubmissionConfig.validation
    }
