module Wizard.Settings.Template.Update exposing
    ( fetchData
    , update
    )

import Shared.Api.Templates as TemplatesApi
import Shared.Data.BootstrapConfig.TemplateConfig as TemplateConfig
import Shared.Data.EditableConfig as EditableConfig
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setTemplates)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Template.Models exposing (Model)
import Wizard.Settings.Template.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ Cmd.map GenericMsg <| GenericUpdate.fetchData appState
        , TemplatesApi.getTemplatesAll appState GetTemplatesComplete
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GenericMsg genericMsg ->
            let
                updateProps =
                    { initForm = .template >> TemplateConfig.initForm
                    , formToConfig = EditableConfig.updateTemplate
                    , formValidation = TemplateConfig.validation
                    }

                ( genericModel, cmd ) =
                    GenericUpdate.update updateProps (wrapMsg << GenericMsg) genericMsg appState model.genericModel
            in
            ( { model | genericModel = genericModel }, cmd )

        GetTemplatesComplete result ->
            applyResult
                { setResult = setTemplates
                , defaultError = lg "apiError.templates.getListError" appState
                , model = model
                , result = result
                }
