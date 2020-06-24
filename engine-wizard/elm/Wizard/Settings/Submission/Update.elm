module Wizard.Settings.Submission.Update exposing
    ( fetchData
    , update
    )

import Shared.Api.Templates as TemplatesApi
import Shared.Data.EditableConfig as EditableConfig
import Shared.Data.EditableConfig.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setTemplates)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
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
