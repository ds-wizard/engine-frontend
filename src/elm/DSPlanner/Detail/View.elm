module DSPlanner.Detail.View exposing (..)

import Common.Html exposing (emptyNode, linkTo)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.Types exposing (ActionResult, combine)
import Common.View exposing (fullPageActionResultView, pageHeader)
import Common.View.Forms exposing (actionButton, formResultView)
import DSPlanner.Detail.Models exposing (Model)
import DSPlanner.Detail.Msgs exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Common.Models.Entities exposing (Level)
import Msgs


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    fullPageActionResultView (content wrapMsg model) <| combine model.questionnaireModel model.levels


content : (Msg -> Msgs.Msg) -> Model -> ( Common.Questionnaire.Models.Model, List Level ) -> Html Msgs.Msg
content wrapMsg model ( questionnaireModel, levels ) =
    let
        questionnaireCfg =
            { showExtraActions = True
            , levels = Just levels
            }
    in
    div [ class "col DSPlanner__Detail" ]
        [ questionnaireHeader wrapMsg model.savingQuestionnaire questionnaireModel
        , formResultView model.savingQuestionnaire
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
                , actionButton ( "Save", savingQuestionnaire, wrapMsg <| Save )
                ]
            ]
        ]


questionnaireTitle : QuestionnaireDetail -> String
questionnaireTitle questionnaire =
    questionnaire.name ++ " (" ++ questionnaire.package.name ++ ", " ++ questionnaire.package.version ++ ")"
