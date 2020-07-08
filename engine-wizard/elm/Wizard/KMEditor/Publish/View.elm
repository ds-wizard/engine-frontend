module Wizard.KMEditor.Publish.View exposing (view)

import Form exposing (Form)
import Form.Field exposing (Field, FieldValue(..))
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared.Data.BranchDetail as BranchDetail exposing (BranchDetail)
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l, lh, lx)
import Shared.Utils exposing (flip)
import Version exposing (Version)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
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
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Publish.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.KMEditor.Publish.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Publish.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (contentView appState model) model.branch


contentView : AppState -> Model -> BranchDetail -> Html Msg
contentView appState model branch =
    div [ wideDetailClass "KMEditor__Publish" ]
        [ Page.header (l_ "header" appState) []
        , div []
            [ FormResult.view appState model.publishingBranch
            , formView appState model.form branch
            , FormActions.view appState
                (Routes.KMEditorRoute IndexRoute)
                (ActionButton.ButtonConfig (l_ "action" appState) model.publishingBranch (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form FormError BranchPublishForm -> BranchDetail -> Html Msg
formView appState form branch =
    let
        mbVersion =
            BranchUtils.lastVersion appState branch
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
