module KMEditor.Publish.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Html.Attribute exposing (wideDetailClass)
import Common.Locale exposing (l, lh, lx)
import Common.View.ActionButton as ActionButton
import Common.View.FormActions as FormActions
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Form.Field exposing (Field, FieldValue(..))
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Common.BranchDetail as BranchDetail exposing (BranchDetail)
import KMEditor.Common.BranchPublishForm exposing (BranchPublishForm)
import KMEditor.Common.BranchUtils as BranchUtils
import KMEditor.Publish.Models exposing (Model)
import KMEditor.Publish.Msgs exposing (Msg(..))
import KMEditor.Routes exposing (Route(..))
import KnowledgeModels.Common.Version as Version exposing (Version)
import Routes
import Utils exposing (flip)


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Publish.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "KMEditor.Publish.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "KMEditor.Publish.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (contentView appState model) model.branch


contentView : AppState -> Model -> BranchDetail -> Html Msg
contentView appState model branch =
    div [ wideDetailClass "KMEditor__Publish" ]
        [ Page.header (l_ "header" appState) []
        , div []
            [ FormResult.view model.publishingBranch
            , formView appState model.form branch
            , FormActions.view appState
                (Routes.KMEditorRoute IndexRoute)
                (ActionButton.ButtonConfig (l_ "action" appState) model.publishingBranch (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form CustomFormError BranchPublishForm -> BranchDetail -> Html Msg
formView appState form branch =
    let
        mbVersion =
            BranchUtils.lastVersion branch
    in
    div []
        [ Html.map FormMsg <| FormGroup.textView branch.name <| l_ "form.name" appState
        , Html.map FormMsg <| FormGroup.codeView branch.kmId <| l_ "form.kmId" appState
        , lastVersion appState mbVersion
        , versionInputGroup appState form mbVersion
        , Html.map FormMsg <| FormGroup.input appState form "license" <| l_ "form.license" appState
        , FormExtra.blockAfter <| lh_ "form.license.description" [ a [ href "https://spdx.org/licenses/", target "_blank" ] [ text (l_ "form.license.description.license" appState) ] ] appState
        , Html.map FormMsg <| FormGroup.input appState form "description" <| l_ "form.description" appState
        , FormExtra.textAfter <| l_ "form.description.description" appState
        , Html.map FormMsg <| FormGroup.markdownEditor appState form "readme" <| l_ "form.readme" appState
        , FormExtra.textAfter <| l_ "form.readme.description" appState
        ]


lastVersion : AppState -> Maybe Version -> Html msg
lastVersion appState mbVersion =
    mbVersion
        |> Maybe.map Version.toString
        |> Maybe.withDefault (l_ "form.lastVersion.empty" appState)
        |> flip FormGroup.textView (l_ "form.lastVersion" appState)


versionInputGroup : AppState -> Form CustomFormError BranchPublishForm -> Maybe Version -> Html Msg
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
        [ label [ class "control-label" ] [ lx_ "form.newVersion" appState ]
        , div [ class "version-inputs" ]
            [ Html.map FormMsg <| Input.baseInput "number" String Form.Text majorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0" ]
            , text "."
            , Html.map FormMsg <| Input.baseInput "number" String Form.Text minorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0" ]
            , text "."
            , Html.map FormMsg <| Input.baseInput "number" String Form.Text patchField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0" ]
            ]
        , p [ class "form-text text-muted version-suggestions" ]
            [ lx_ "form.newVersion.suggestions" appState
            , a [ onClick <| FormSetVersion nextMajor ] [ text <| Version.toString nextMajor ]
            , a [ onClick <| FormSetVersion nextMinor ] [ text <| Version.toString nextMinor ]
            , a [ onClick <| FormSetVersion nextPatch ] [ text <| Version.toString nextPatch ]
            ]
        , FormExtra.text <| l_ "form.newVersion.description" appState
        ]
