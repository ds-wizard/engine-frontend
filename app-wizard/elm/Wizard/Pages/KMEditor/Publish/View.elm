module Wizard.Pages.KMEditor.Publish.View exposing (view)

import Flip exposing (flip)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href, target)
import Html.Attributes.Extensions exposing (dataCy)
import Shared.Components.ActionButton as ActionButton
import Shared.Components.FormExtra as FormExtra
import Shared.Components.FormGroup as FormGroup
import Shared.Components.FormResult as FormResult
import Shared.Components.Page as Page
import Shared.Utils.Form.FormError exposing (FormError)
import String.Format as String
import Version exposing (Version)
import Wizard.Api.Models.BranchDetail exposing (BranchDetail)
import Wizard.Components.FormActions as FormActions
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Common.BranchPublishForm exposing (BranchPublishForm)
import Wizard.Pages.KMEditor.Common.BranchUtils as BranchUtils
import Wizard.Pages.KMEditor.Publish.Models exposing (Model)
import Wizard.Pages.KMEditor.Publish.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (wideDetailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (contentView appState model) model.branch


contentView : AppState -> Model -> BranchDetail -> Html Msg
contentView appState model branch =
    div [ wideDetailClass "KMEditor__Publish" ]
        [ Page.header (gettext "Publish new version" appState.locale) []
        , div []
            [ FormResult.view model.publishingBranch
            , formView appState model.form branch
            , FormActions.viewCustomButton appState
                Cancel
                (ActionButton.buttonWithAttrs
                    (ActionButton.ButtonWithAttrsConfig (gettext "Publish" appState.locale)
                        model.publishingBranch
                        (FormMsg Form.Submit)
                        False
                        [ dataCy "km-publish_publish-button" ]
                    )
                )
            ]
        ]


formView : AppState -> Form FormError BranchPublishForm -> BranchDetail -> Html Msg
formView appState form branch =
    let
        mbVersion =
            BranchUtils.lastVersion appState branch

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
        [ Html.map FormMsg <| FormGroup.textView "name" branch.name <| gettext "Knowledge Model" appState.locale
        , Html.map FormMsg <| FormGroup.codeView branch.kmId <| gettext "Knowledge Model ID" appState.locale
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
