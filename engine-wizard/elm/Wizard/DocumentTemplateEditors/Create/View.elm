module Wizard.DocumentTemplateEditors.Create.View exposing (view)

import ActionResult
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Events exposing (onSubmit)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.DocumentTemplateEditors.Create.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Create.Msgs exposing (Msg(..))


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
    div [ detailClass "" ]
        [ Page.header (gettext "Create Document Template" appState.locale) []
        , Html.form [ onSubmit (FormMsg Form.Submit) ]
            [ FormResult.errorOnlyView appState model.savingDocumentTemplate
            , formView appState model
            , FormActions.viewSubmit appState
                Cancel
                (ActionButton.SubmitConfig (gettext "Create" appState.locale) model.savingDocumentTemplate)
            ]
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
                            { viewItem = TypeHintItem.templateSuggestion
                            , wrapMsg = DocumentTemplateTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = True
                            }

                        typeHintInput =
                            TypeHintInput.view appState cfg model.documentTemplateTypeHintInputModel
                    in
                    FormGroup.formGroupCustom typeHintInput appState model.form "basedOn"

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
        [ Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Name" appState.locale
        , Html.map FormMsg <| FormGroup.input appState model.form "templateId" <| gettext "Document Template ID" appState.locale
        , FormExtra.textAfter <| gettext "Document template ID can contain alphanumeric characters and dashes but cannot start or end with a dash." appState.locale
        , FormGroup.version appState versionInputConfig model.form
        , parentInput <| gettext "Based on" appState.locale
        , FormExtra.textAfter <| gettext "You can create a new document template based on the existing one or start from scratch." appState.locale
        ]
