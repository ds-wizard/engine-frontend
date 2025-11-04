module Wizard.Pages.KMEditor.Create.View exposing (view)

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
import Wizard.Pages.KMEditor.Create.Models exposing (Model)
import Wizard.Pages.KMEditor.Create.Msgs exposing (Msg(..))
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
    Container.simpleForm
        [ Page.headerWithGuideLink
            (AppState.toGuideLinkConfig appState WizardGuideLinks.kmEditorCreate)
            (gettext "Create knowledge model" appState.locale)
        , Form.viewSimple
            { formMsg = FormMsg
            , formResult = model.savingBranch
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
            case model.selectedPackage of
                Just package ->
                    FormGroup.codeView package

                Nothing ->
                    let
                        cfg =
                            { viewItem = TypeHintInputItem.packageSuggestionWithVersion
                            , wrapMsg = PackageTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = True
                            , locale = appState.locale
                            }

                        typeHintInput =
                            TypeHintInput.view cfg model.packageTypeHintInputModel
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
