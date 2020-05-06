module Wizard.Settings.Submission.Update exposing
    ( fetchData
    , update
    )

import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.Api.Templates as TemplatesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Setters exposing (setTemplates)
import Wizard.Msgs
import Wizard.Settings.Common.EditableConfig as EditableConfig
import Wizard.Settings.Common.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Submission.Models exposing (Model)
import Wizard.Settings.Submission.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ TemplatesApi.getTemplates appState GetTemplatesCompleted
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
            applyResult
                { setResult = setTemplates
                , defaultError = lg "apiError.templates.getListError" appState
                , model = model
                , result = result
                }


updateProps : GenericUpdate.UpdateProps EditableSubmissionConfig
updateProps =
    { initForm = .submission >> EditableSubmissionConfig.initForm
    , formToConfig = EditableConfig.updateSubmission
    , formValidation = EditableSubmissionConfig.validation
    }
