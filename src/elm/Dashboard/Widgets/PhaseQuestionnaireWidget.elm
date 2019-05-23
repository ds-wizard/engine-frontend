module Dashboard.Widgets.PhaseQuestionnaireWidget exposing (view)

import ActionResult exposing (ActionResult)
import Common.Html exposing (emptyNode, linkTo)
import Common.View.Listing as Listing exposing (ListingConfig)
import Common.View.Page as Page
import Html exposing (Html, code, div, h3, span, strong, text)
import Html.Attributes exposing (class, title)
import KMEditor.Common.Models.Entities exposing (Level)
import KnowledgeModels.Common.Version as Version
import KnowledgeModels.Routing
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Common.View exposing (accessibilityBadge)
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (Route(..))


view : ActionResult (List Level) -> ActionResult (List Questionnaire) -> Html msg
view levels questionnaires =
    Page.actionResultView viewQuestionnaires <| ActionResult.combine questionnaires levels


viewQuestionnaires : ( List Questionnaire, List Level ) -> Html msg
viewQuestionnaires ( questionnaires, levels ) =
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
                Listing.view listingConfig questionnaires

            else
                div [ class "empty" ]
                    [ text "No questionnaires in this phase" ]
    in
    div []
        [ strong [] [ text level.title ]
        , content
        ]


listingConfig : ListingConfig Questionnaire msg
listingConfig =
    { title = listingTitle
    , description = listingDescription
    , actions = always []
    , textTitle = .name
    , emptyText = "Click \"Create\" button to add a new Questionnaire."
    }


listingTitle : Questionnaire -> Html msg
listingTitle questionnaire =
    span []
        [ linkTo (Routing.Questionnaires <| Detail <| questionnaire.uuid) [] [ text questionnaire.name ]
        , accessibilityBadge questionnaire.accessibility
        ]


listingDescription : Questionnaire -> Html msg
listingDescription questionnaire =
    let
        kmRoute =
            Routing.KnowledgeModels <|
                KnowledgeModels.Routing.Detail questionnaire.package.id
    in
    linkTo kmRoute
        [ title "Knowledge Model" ]
        [ text questionnaire.package.name
        , text ", "
        , text <| Version.toString questionnaire.package.version
        , text " ("
        , code [] [ text questionnaire.package.id ]
        , text ")"
        ]
