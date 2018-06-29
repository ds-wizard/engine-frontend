module DSPlanner.Detail.View exposing (..)

import Common.Html exposing (emptyNode, linkTo)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.Types exposing (ActionResult)
import Common.View exposing (fullPageActionResultView, pageHeader)
import Common.View.Forms exposing (actionButton, formResultView)
import DSPlanner.Detail.Models exposing (Model)
import DSPlanner.Detail.Msgs exposing (Msg(..))
import DSPlanner.Routing exposing (Route(Index))
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    fullPageActionResultView (content wrapMsg model) model.questionnaireModel


content : (Msg -> Msgs.Msg) -> Model -> Common.Questionnaire.Models.Model -> Html Msgs.Msg
content wrapMsg model questionnaireModel =
    div [ class "col DSPlanner__Detail" ]
        [ pageHeader (questionnaireTitle questionnaireModel.questionnaire) (actions wrapMsg model.savingQuestionnaire)
        , formResultView model.savingQuestionnaire
        , viewQuestionnaire questionnaireModel |> Html.map (QuestionnaireMsg >> wrapMsg)
        ]


questionnaireTitle : QuestionnaireDetail -> String
questionnaireTitle questionnaire =
    questionnaire.name ++ " (" ++ questionnaire.package.name ++ ", " ++ questionnaire.package.version ++ ")"


actions : (Msg -> Msgs.Msg) -> ActionResult String -> List (Html Msgs.Msg)
actions wrapMsg savingQuestionnaire =
    [ linkTo (Routing.DSPlanner Index) [ class "btn btn-secondary" ] [ text "Cancel" ]
    , actionButton ( "Save", savingQuestionnaire, wrapMsg <| Save )
    ]
