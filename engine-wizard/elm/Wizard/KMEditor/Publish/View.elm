module Wizard.KMEditor.Publish.View exposing (view)

import Form exposing (Form)
import Form.Field exposing (FieldValue(..))
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, div, label, p, text)
import Html.Attributes exposing (class, href, id, name, target)
import Html.Events exposing (onClick)
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
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.KMEditor.Publish.Models exposing (Model)
import Wizard.KMEditor.Publish.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes as KMEditorRoutes
import Wizard.Routes as Routes


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
                (Routes.KMEditorRoute (KMEditorRoutes.EditorRoute branch.uuid (KMEditorRoute.Edit Nothing)))
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
    in
    div []
        [ Html.map FormMsg <| FormGroup.textView "name" branch.name <| gettext "Knowledge Model" appState.locale
        , Html.map FormMsg <| FormGroup.codeView branch.kmId <| gettext "Knowledge Model ID" appState.locale
        , lastVersion appState mbVersion
        , versionInputGroup appState form mbVersion
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
        , FormExtra.textAfter <| gettext "Describe the Knowledge Model, you can use Markdown." appState.locale
        ]


lastVersion : AppState -> Maybe Version -> Html msg
lastVersion appState mbVersion =
    mbVersion
        |> Maybe.map Version.toString
        |> Maybe.withDefault (gettext "No version of this package has been published yet." appState.locale)
        |> flip (FormGroup.textView "last-version") (gettext "Last version" appState.locale)


versionInputGroup : AppState -> Form FormError BranchPublishForm -> Maybe Version -> Html Msg
versionInputGroup appState form mbVersion =
    let
        majorField =
            Form.getFieldAsString "major" form

        minorField =
            Form.getFieldAsString "minor" form

        patchField =
            Form.getFieldAsString "patch" form

        errorClass =
            case ( majorField.liveError, minorField.liveError, patchField.liveError ) of
                ( Nothing, Nothing, Nothing ) ->
                    ""

                _ ->
                    " is-invalid"

        nextMajor =
            mbVersion
                |> Maybe.map Version.nextMajor
                |> Maybe.withDefault (Version.create 1 0 0)

        nextMinor =
            mbVersion
                |> Maybe.map Version.nextMinor
                |> Maybe.withDefault (Version.create 0 1 0)

        nextPatch =
            mbVersion
                |> Maybe.map Version.nextPatch
                |> Maybe.withDefault (Version.create 0 0 1)
    in
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text (gettext "New version" appState.locale) ]
        , div [ class "version-inputs" ]
            [ Html.map FormMsg <| Input.baseInput "number" String Form.Text majorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0", name "version-major", id "version-major" ]
            , text "."
            , Html.map FormMsg <| Input.baseInput "number" String Form.Text minorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0", name "version-minor", id "version-minor" ]
            , text "."
            , Html.map FormMsg <| Input.baseInput "number" String Form.Text patchField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0", name "version-patch", id "version-patch" ]
            ]
        , p [ class "form-text text-muted version-suggestions" ]
            [ text (gettext "Suggestions: " appState.locale)
            , a [ onClick <| FormSetVersion nextMajor ] [ text <| Version.toString nextMajor ]
            , a [ onClick <| FormSetVersion nextMinor ] [ text <| Version.toString nextMinor ]
            , a [ onClick <| FormSetVersion nextPatch ] [ text <| Version.toString nextPatch ]
            ]
        , FormExtra.text <| gettext "The version number is in format X.Y.Z. Increasing number Z indicates only some fixes, number Y minor changes, and number X indicates a major change." appState.locale
        ]
