module Wizard.Projects.Create.View exposing (view)

import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Shared.Locale exposing (l, lg)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.ItemIcon as ItemIcon
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
    div [ detailClass "Questionnaires__Create" ]
        [ Page.header (l_ "header.title" appState) []
        , div [ onSubmit (FormMsg Form.Submit) ]
            [ FormResult.view appState model.savingQuestionnaire
            , formView appState model
            , tagsView appState model
            , FormActions.viewSubmit appState
                Routes.projectsIndex
                (ActionResult.SubmitConfig (l_ "header.save" appState) model.savingQuestionnaire)
            ]
        ]


formView : AppState -> Model -> Html Msg
formView appState model =
    let
        cfg =
            { viewItem = TypeHintItem.packageSuggestion
            , wrapMsg = PackageTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = True
            }

        typeHintInput =
            TypeHintInput.view appState cfg model.packageTypeHintInputModel

        parentInput =
            case model.selectedPackage of
                Just package ->
                    FormGroup.codeView package

                Nothing ->
                    FormGroup.formGroupCustom typeHintInput appState model.form "packageId"
    in
    div []
        [ Html.map FormMsg <| FormGroup.input appState model.form "name" <| lg "questionnaire.name" appState
        , parentInput <| lg "knowledgeModel" appState
        ]


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
