module Dashboard.Widgets.PhaseQuestionnaireWidget exposing (view)

import ActionResult exposing (ActionResult)
import Common.Html exposing (emptyNode)
import Common.View.Page as Page
import Html exposing (Html, a, code, div, h3, span, strong, text)
import Html.Attributes exposing (class, href)
import KMEditor.Common.Models.Entities exposing (Level)
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (Route(..))


view : ActionResult (List Level) -> ActionResult (List Questionnaire) -> Html msg
view levels questionnaires =
    Page.actionResultView viewQuestionnaires <| ActionResult.combine levels questionnaires


viewQuestionnaires : ( List Level, List Questionnaire ) -> Html msg
viewQuestionnaires ( levels, questionnaires ) =
    if List.length questionnaires > 0 then
        div [ class "PhaseQuestionnaireWidget" ]
            [ h3 [] [ text "Your questionnaires" ]
            , div [] (List.map (viewLevelGroup questionnaires) levels)
            ]

    else
        emptyNode


viewLevelGroup : List Questionnaire -> Level -> Html msg
viewLevelGroup questionnaires level =
    let
        levelQuestionnaires =
            List.sortBy .name <| List.filter (.level >> (==) level.level) questionnaires
    in
    viewLevelGroupWithData levelQuestionnaires level


viewLevelGroupWithData : List Questionnaire -> Level -> Html msg
viewLevelGroupWithData questionnaires level =
    let
        content =
            if List.length questionnaires > 0 then
                div [ class "list-group list-group-flush" ]
                    (List.map viewQuestionnaire questionnaires)

            else
                div [ class "empty" ]
                    [ text "No questionnaires in this phase" ]
    in
    div []
        [ strong [] [ text level.title ]
        , content
        ]


viewQuestionnaire : Questionnaire -> Html msg
viewQuestionnaire questionnaire =
    a [ class "list-group-item", href <| Routing.toUrl <| Questionnaires <| Detail questionnaire.uuid ]
        [ span [ class "name" ]
            [ span [] [ text questionnaire.name ]
            , span [ class "km" ]
                [ text questionnaire.package.name
                , text ", "
                , text questionnaire.package.version
                , text " ("
                , code [] [ text questionnaire.package.id ]
                , text ")"
                ]
            ]
        , badge questionnaire
        ]


badge : Questionnaire -> Html msg
badge questionnaire =
    if questionnaire.private then
        span [ class "badge badge-danger" ]
            [ text "private" ]

    else
        span [ class "badge badge-info" ]
            [ text "public" ]
