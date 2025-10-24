module Wizard.Pages.KMEditor.Publish.View exposing (view)

import Common.Components.ActionButton as ActionButton
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Utils.Form.FormError exposing (FormError)
import Flip exposing (flip)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href, target)
import Html.Attributes.Extensions exposing (dataCy)
import String.Format as String
import Version exposing (Version)
import Wizard.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Components.FormActions as FormActions
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorPublishForm exposing (KnowledgeModelEditorPublishForm)
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorUtils as KnowledgeModelEditorUtils
import Wizard.Pages.KMEditor.Publish.Models exposing (Model)
import Wizard.Pages.KMEditor.Publish.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (wideDetailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (contentView appState model) model.kmEditor


contentView : AppState -> Model -> KnowledgeModelEditorDetail -> Html Msg
contentView appState model kmEditor =
    div [ wideDetailClass "KMEditor__Publish" ]
        [ Page.header (gettext "Publish new version" appState.locale) []
        , div []
            [ FormResult.view model.publishingKnowledgeModelEditor
            , formView appState model.form kmEditor
            , FormActions.viewCustomButton appState
                Cancel
                (ActionButton.buttonWithAttrs
                    (ActionButton.ButtonWithAttrsConfig (gettext "Publish" appState.locale)
                        model.publishingKnowledgeModelEditor
                        (FormMsg Form.Submit)
                        False
                        [ dataCy "km-publish_publish-button" ]
                    )
                )
            ]
        ]


formView : AppState -> Form FormError KnowledgeModelEditorPublishForm -> KnowledgeModelEditorDetail -> Html Msg
formView appState form kmEditor =
    let
        mbVersion =
            KnowledgeModelEditorUtils.lastVersion appState kmEditor

        versionInputConfig =
            { label = gettext "New version" appState.locale
            , majorField = "major"
            , minorField = "minor"
            , patchField = "patch"
            , currentVersion = mbVersion
            , wrapFormMsg = FormMsg
            , setVersionMsg = Just FormSetVersion
            }
    in
    div []
        [ Html.map FormMsg <| FormGroup.textView "name" kmEditor.name <| gettext "Knowledge Model" appState.locale
        , Html.map FormMsg <| FormGroup.codeView kmEditor.kmId <| gettext "Knowledge Model ID" appState.locale
        , lastVersion appState mbVersion
        , FormGroup.version appState.locale versionInputConfig form
        , Html.map FormMsg <| FormGroup.input appState.locale form "license" <| gettext "License" appState.locale
        , FormExtra.blockAfter <|
            String.formatHtml
                (gettext "Choose a %s so others can use your knowledge model." appState.locale)
                [ a [ href "https://spdx.org/licenses/", target "_blank" ]
                    [ text (gettext "license" appState.locale) ]
                ]
        , Html.map FormMsg <| FormGroup.input appState.locale form "description" <| gettext "Description" appState.locale
        , FormExtra.textAfter <| gettext "Short description of the knowledge model." appState.locale
        , Html.map FormMsg <| FormGroup.markdownEditor appState.locale (WizardGuideLinks.markdownCheatsheet appState.guideLinks) form "readme" <| gettext "Readme" appState.locale
        ]


lastVersion : AppState -> Maybe Version -> Html msg
lastVersion appState mbVersion =
    mbVersion
        |> Maybe.map Version.toString
        |> Maybe.withDefault (gettext "No version of this package has been published yet." appState.locale)
        |> flip (FormGroup.textView "last-version") (gettext "Last version" appState.locale)
