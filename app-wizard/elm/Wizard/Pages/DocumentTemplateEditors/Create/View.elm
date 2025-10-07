module Wizard.Pages.DocumentTemplateEditors.Create.View exposing (view)

import ActionResult
import Common.Components.Container as Container
import Common.Components.Form as Form
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Components.TypeHintInput as TypeHintInput
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Wizard.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.DocumentTemplateEditors.Create.Models exposing (Model)
import Wizard.Pages.DocumentTemplateEditors.Create.Msgs exposing (Msg(..))
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    let
        pageView =
            Page.actionResultView appState (viewCreate appState model)
    in
    case ( model.selectedDocumentTemplate, model.edit ) of
        ( Just _, True ) ->
            pageView model.documentTemplate

        _ ->
            pageView (ActionResult.Success ())


viewCreate : AppState -> Model -> a -> Html Msg
viewCreate appState model _ =
    Container.simpleForm
        [ Page.headerWithGuideLink
            (AppState.toGuideLinkConfig appState WizardGuideLinks.documentTemplatesCreate)
            (gettext "Create Document Template" appState.locale)
        , Form.simple
            { formMsg = FormMsg
            , formResult = model.savingDocumentTemplate
            , formView = formView appState model
            , submitLabel = gettext "Create" appState.locale
            , cancelMsg = Just Cancel
            , locale = appState.locale
            , isMac = appState.navigator.isMac
            }
        ]


formView : AppState -> Model -> Html Msg
formView appState model =
    let
        parentInput =
            case model.selectedDocumentTemplate of
                Just documentTemplate ->
                    FormGroup.codeView documentTemplate

                Nothing ->
                    let
                        cfg =
                            { viewItem = TypeHintInputItem.templateSuggestion
                            , wrapMsg = DocumentTemplateTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = True
                            , locale = appState.locale
                            }

                        typeHintInput =
                            TypeHintInput.view cfg model.documentTemplateTypeHintInputModel
                    in
                    FormGroup.formGroupCustom typeHintInput appState.locale model.form "basedOn"

        previousVersion =
            model.documentTemplate
                |> ActionResult.toMaybe
                |> Maybe.map .version

        versionInputConfig =
            { label = gettext "New version" appState.locale
            , majorField = "versionMajor"
            , minorField = "versionMinor"
            , patchField = "versionPatch"
            , currentVersion = previousVersion
            , wrapFormMsg = FormMsg
            , setVersionMsg = Just FormSetVersion
            }
    in
    div []
        [ Html.map FormMsg <| FormGroup.input appState.locale model.form "name" <| gettext "Name" appState.locale
        , Html.map FormMsg <| FormGroup.input appState.locale model.form "templateId" <| gettext "Document Template ID" appState.locale
        , FormExtra.textAfter <| gettext "Document template ID can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale
        , FormGroup.version appState.locale versionInputConfig model.form
        , parentInput <| gettext "Based on" appState.locale
        , FormExtra.textAfter <| gettext "You can create a new document template based on the existing one or start from scratch." appState.locale
        ]
