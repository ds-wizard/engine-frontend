module DSPlanner.Detail.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (emptyNode)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import DSPlanner.Detail.Models exposing (Model)
import DSPlanner.Detail.Msgs exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Common.Models.Entities exposing (Level)
import Msgs


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    Page.actionResultView (content wrapMsg model) <| ActionResult.combine model.questionnaireModel model.levels


content : (Msg -> Msgs.Msg) -> Model -> ( Common.Questionnaire.Models.Model, List Level ) -> Html Msgs.Msg
content wrapMsg model ( questionnaireModel, levels ) =
    let
        questionnaireCfg =
            { showExtraActions = True
            , showExtraNavigation = True
            , levels = Just levels
            }
    in
    div [ class "col DSPlanner__Detail" ]
        [ questionnaireHeader wrapMsg model.savingQuestionnaire questionnaireModel
        , FormResult.view model.savingQuestionnaire
        , viewQuestionnaire questionnaireCfg questionnaireModel |> Html.map (QuestionnaireMsg >> wrapMsg)
        ]


questionnaireHeader : (Msg -> Msgs.Msg) -> ActionResult String -> Common.Questionnaire.Models.Model -> Html Msgs.Msg
questionnaireHeader wrapMsg savingQuestionnaire questionnaireModel =
    let
        unsavedChanges =
            if questionnaireModel.dirty then
                text "(unsaved changes)"

            else
                emptyNode
    in
    div [ class "questionnaire-header" ]
        [ div [ class "questionnaire-header-content" ]
            [ text <| questionnaireTitle questionnaireModel.questionnaire
            , div []
                [ unsavedChanges
                , ActionButton.button ( "Save", savingQuestionnaire, wrapMsg <| Save )
                ]
            ]
        ]


questionnaireTitle : QuestionnaireDetail -> String
questionnaireTitle questionnaire =
    questionnaire.name ++ " (" ++ questionnaire.package.name ++ ", " ++ questionnaire.package.version ++ ")"
