module Wizard.KMEditor.Publish.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href, target)
import Shared.Data.BranchDetail exposing (BranchDetail)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils exposing (flip)
import String.Format as String
import Version exposing (Version)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, wideDetailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.BranchPublishForm exposing (BranchPublishForm)
import Wizard.KMEditor.Common.BranchUtils as BranchUtils
import Wizard.KMEditor.Publish.Models exposing (Model)
import Wizard.KMEditor.Publish.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (contentView appState model) model.branch


contentView : AppState -> Model -> BranchDetail -> Html Msg
contentView appState model branch =
    div [ wideDetailClass "KMEditor__Publish" ]
        [ Page.header (gettext "Publish new version" appState.locale) []
        , div []
            [ FormResult.view appState model.publishingBranch
            , formView appState model.form branch
            , FormActions.viewCustomButton appState
                Cancel
                (ActionButton.buttonWithAttrs appState
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
        , FormGroup.version appState versionInputConfig form
        , Html.map FormMsg <| FormGroup.input appState form "license" <| gettext "License" appState.locale
        , FormExtra.blockAfter <|
            String.formatHtml
                (gettext "Choose a %s so others can use your Knowledge Model." appState.locale)
                [ a [ href "https://spdx.org/licenses/", target "_blank" ]
                    [ text (gettext "license" appState.locale) ]
                ]
        , Html.map FormMsg <| FormGroup.input appState form "description" <| gettext "Description" appState.locale
        , FormExtra.textAfter <| gettext "Short description of the Knowledge Model." appState.locale
        , Html.map FormMsg <| FormGroup.markdownEditor appState form "readme" <| gettext "Readme" appState.locale
        ]


lastVersion : AppState -> Maybe Version -> Html msg
lastVersion appState mbVersion =
    mbVersion
        |> Maybe.map Version.toString
        |> Maybe.withDefault (gettext "No version of this package has been published yet." appState.locale)
        |> flip (FormGroup.textView "last-version") (gettext "Last version" appState.locale)
