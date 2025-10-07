module Wizard.Pages.KMEditor.Create.View exposing (view)

import ActionResult
import Common.Components.ActionButton as ActionButton
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Events exposing (onSubmit)
import Wizard.Components.FormActions as FormActions
import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.KMEditor.Create.Models exposing (Model)
import Wizard.Pages.KMEditor.Create.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (detailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    let
        pageView =
            Page.actionResultView appState (viewCreate appState model)
    in
    case ( model.selectedPackage, model.edit ) of
        ( Just _, True ) ->
            pageView model.package

        _ ->
            pageView (ActionResult.Success ())


viewCreate : AppState -> Model -> a -> Html Msg
viewCreate appState model _ =
    div [ detailClass "KMEditor__Create" ]
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.kmEditorCreate) (gettext "Create knowledge model" appState.locale)
        , Html.form [ onSubmit (FormMsg Form.Submit) ]
            [ FormResult.errorOnlyView model.savingBranch
            , formView appState model
            , FormActions.viewSubmit appState
                Cancel
                (ActionButton.SubmitConfig (gettext "Create" appState.locale) model.savingBranch)
            ]
        ]


formView : AppState -> Model -> Html Msg
formView appState model =
    let
        parentInput =
            case model.selectedPackage of
                Just package ->
                    FormGroup.codeView package

                Nothing ->
                    let
                        cfg =
                            { viewItem = TypeHintItem.packageSuggestionWithVersion
                            , wrapMsg = PackageTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = True
                            }

                        typeHintInput =
                            TypeHintInput.view appState cfg model.packageTypeHintInputModel
                    in
                    FormGroup.formGroupCustom typeHintInput appState.locale model.form "previousPackageId"

        previousVersion =
            if model.edit then
                model.package
                    |> ActionResult.toMaybe
                    |> Maybe.map .version

            else
                Nothing

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
        , Html.map FormMsg <| FormGroup.input appState.locale model.form "kmId" <| gettext "Knowledge Model ID" appState.locale
        , FormExtra.textAfter <| gettext "Knowledge model ID can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale
        , FormGroup.version appState.locale versionInputConfig model.form
        , parentInput <| gettext "Based on" appState.locale
        , FormExtra.textAfter <| gettext "You can create a new knowledge model based on the existing one or start from scratch." appState.locale
        ]
