module KMEditor.Create.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html.Attribute exposing (detailClass)
import Common.Locale exposing (l, lg)
import Common.View.ActionButton as ActionButton
import Common.View.FormActions as FormActions
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import KMEditor.Create.Models exposing (..)
import KMEditor.Create.Msgs exposing (Msg(..))
import KMEditor.Routes exposing (Route(..))
import KnowledgeModels.Common.Package as Package exposing (Package)
import Routes


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Create.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (content appState model) model.packages


content : AppState -> Model -> List Package -> Html Msg
content appState model packages =
    div [ detailClass "KMEditor__Create" ]
        [ Page.header (l_ "header" appState) []
        , div []
            [ FormResult.errorOnlyView model.savingBranch
            , formView appState model packages
            , FormActions.view appState
                (Routes.KMEditorRoute IndexRoute)
                (ActionButton.ButtonConfig (l_ "save" appState) model.savingBranch (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Model -> List Package -> Html Msg
formView appState model packages =
    let
        parentOptions =
            ( "", "--" ) :: (List.map Package.createFormOption <| List.sortBy .name packages)

        parentInput =
            case model.selectedPackage of
                Just package ->
                    FormGroup.codeView package

                Nothing ->
                    FormGroup.select appState parentOptions model.form "previousPackageId"

        formHtml =
            div []
                [ FormGroup.input appState model.form "name" <| lg "branch.name" appState
                , FormGroup.input appState model.form "kmId" <| lg "branch.kmId" appState
                , FormExtra.textAfter <| l_ "form.kmIdHint" appState
                , parentInput <| lg "branch.basedOn" appState
                , FormExtra.textAfter <| l_ "form.basedOnHint" appState
                ]
    in
    formHtml |> Html.map FormMsg
