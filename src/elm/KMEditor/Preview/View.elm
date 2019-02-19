module KMEditor.Preview.View exposing (view)

import ActionResult
import Common.Html exposing (emptyNode, linkTo)
import Common.Questionnaire.Models
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.Page as Page
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import KMEditor.Common.Models.Entities exposing (Level)
import KMEditor.Preview.Models exposing (Model)
import KMEditor.Preview.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    ActionResult.combine model.questionnaireModel model.levels
        |> Page.actionResultView (content wrapMsg model)


content : (Msg -> Msgs.Msg) -> Model -> ( Common.Questionnaire.Models.Model, List Level ) -> Html Msgs.Msg
content wrapMsg model ( questionnaireModel, levels ) =
    let
        questionnaire =
            viewQuestionnaire
                { showExtraActions = False
                , levels = Just levels
                }
                questionnaireModel
                |> Html.map (QuestionnaireMsg >> wrapMsg)
    in
    div [ class "col KMEditor__Preview" ]
        [ questionnaireHeader model.branchUuid
        , questionnaire
        ]


questionnaireHeader : String -> Html Msgs.Msg
questionnaireHeader uuid =
    div [ class "questionnaire-header" ]
        [ div [ class "questionnaire-header-content" ]
            [ text "Questionnaire Preview"
            , div []
                [ linkTo (KMEditor <| EditorRoute uuid) [ class "btn btn-primary" ] [ text "Edit Knowledge Model" ] ]
            ]
        ]
