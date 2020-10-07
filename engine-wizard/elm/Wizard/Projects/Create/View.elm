module Wizard.Projects.Create.View exposing (view)

import Form exposing (Form)
import Html exposing (..)
import Shared.Data.Package exposing (Package)
import Shared.Locale exposing (l, lg)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.Tag as Tag
import Wizard.Projects.Create.Models exposing (Model)
import Wizard.Projects.Create.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Create.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (content appState model) model.packages


content : AppState -> Model -> List Package -> Html Msg
content appState model packages =
    div [ detailClass "Questionnaires__Create" ]
        [ Page.header (l_ "header.title" appState) []
        , div []
            [ FormResult.view appState model.savingQuestionnaire
            , formView appState model packages |> Html.map FormMsg
            , tagsView appState model
            , FormActions.view appState
                Routes.projectsIndex
                (ActionResult.ButtonConfig (l_ "header.save" appState) model.savingQuestionnaire (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Model -> List Package -> Html Form.Msg
formView appState model packages =
    let
        packageOptions =
            ( "", "--" ) :: (List.map createOption <| List.sortBy .name packages)

        parentInput =
            case model.selectedPackage of
                Just package ->
                    FormGroup.codeView package

                Nothing ->
                    FormGroup.select appState packageOptions model.form "packageId"

        formHtml =
            div []
                [ FormGroup.input appState model.form "name" <| lg "questionnaire.name" appState
                , parentInput <| lg "knowledgeModel" appState
                ]
    in
    formHtml


tagsView : AppState -> Model -> Html Msg
tagsView appState model =
    let
        tagListConfig =
            { selected = model.selectedTags
            , addMsg = AddTag
            , removeMsg = RemoveTag
            }
    in
    Tag.selection appState tagListConfig model.knowledgeModelPreview


createOption : Package -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ Version.toString package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
