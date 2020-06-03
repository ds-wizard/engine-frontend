module Wizard.Settings.Template.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.Api.Templates as TemplatesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.TemplateConfig as Template
import Wizard.Common.Setters exposing (setTemplates)
import Wizard.Msgs
import Wizard.Settings.Common.EditableConfig as EditableConfig
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Template.Models exposing (Model)
import Wizard.Settings.Template.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ Cmd.map GenericMsg <| GenericUpdate.fetchData appState
        , TemplatesApi.getTemplates appState GetTemplatesComplete
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GenericMsg genericMsg ->
            let
                updateProps =
                    { initForm = .template >> Template.initForm
                    , formToConfig = EditableConfig.updateTemplate
                    , formValidation = Template.validation
                    }

                ( genericModel, cmd ) =
                    GenericUpdate.update updateProps (wrapMsg << GenericMsg) genericMsg appState model.genericModel
            in
            ( { model | genericModel = genericModel }, cmd )

        GetTemplatesComplete result ->
            applyResult
                { setResult = setTemplates

                -- TODO
                , defaultError = "Unable to get templates"
                , model = model
                , result = result
                }
