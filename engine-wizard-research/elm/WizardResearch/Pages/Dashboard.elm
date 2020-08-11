module WizardResearch.Pages.Dashboard exposing (Model, Msg, UpdateConfig, init, update, view)

import ActionResult exposing (ActionResult(..))
import Css exposing (display, inlineBlock, px, width)
import Html.Styled exposing (Html, a, h1, i, span, text)
import Html.Styled.Attributes exposing (class, css, href, title)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Api.Levels as LevelsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.Questionnaire.QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.SummaryReport as SummaryReport
import Shared.Elemental.Atoms.Badge as Badge
import Shared.Elemental.Atoms.Button as Button
import Shared.Elemental.Atoms.ProgressBar as ProgressBar
import Shared.Elemental.Components.ActionResultWrapper as ActionResultWrapper
import Shared.Elemental.Components.Listing as Listing
import Shared.Elemental.Foundations.Animation as Animation
import Shared.Elemental.Foundations.Grid as Grid
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html.Styled exposing (emptyNode, fa)
import WizardResearch.Common.AppState exposing (AppState)
import WizardResearch.Ports as Ports
import WizardResearch.Route as Route exposing (Route)
import WizardResearch.Route.ProjectRoute as ProjectRoute



-- MODEL


type alias Model =
    { questionnaires : Listing.Model Questionnaire
    , levels : ActionResult (List Level)
    }


init : AppState -> PaginationQueryString -> ( Model, Cmd Msg )
init appState paginationQueryString =
    let
        ( questionnaires, questionnairesCmd ) =
            Listing.init paginationQueryString
    in
    ( { questionnaires = questionnaires
      , levels = Loading
      }
    , Cmd.batch
        [ Cmd.map ListingMsg questionnairesCmd
        , LevelsApi.getLevels appState GetLevelsComplete
        ]
    )



-- UPDATE


type Msg
    = ListingMsg (Listing.Msg Questionnaire)
    | GetLevelsComplete (Result ApiError (List Level))


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , cmdNavigate : Route -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ListingMsg listingMsg ->
            let
                updateConfig =
                    { getRequest = QuestionnairesApi.getQuestionnaires
                    , getError = "Unable to get questionnaires"
                    , wrapMsg = cfg.wrapMsg << ListingMsg
                    , updateUrlCmd = Ports.replaceUrl << Route.toString << Route.Dashboard
                    }

                ( questionnaires, questionnairesCmd ) =
                    Listing.update updateConfig appState listingMsg model.questionnaires
            in
            ( { model | questionnaires = questionnaires }
            , questionnairesCmd
            )

        GetLevelsComplete result ->
            case result of
                Ok levels ->
                    ( { model | levels = Success levels }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | levels = ApiError.toActionResult "Unable to get levels" error }
                    , Cmd.none
                    )



-- VIEW


view : AppState -> Model -> { title : String, content : Html Msg }
view appState model =
    { title = "Projects"
    , content = ActionResultWrapper.page appState.theme (viewContent appState model) model.levels
    }


viewContent : AppState -> Model -> List Level -> Html Msg
viewContent appState model levels =
    let
        visibilityIcon questionnaire =
            let
                ( icon, iconTitle ) =
                    case questionnaire.visibility of
                        VisibleEditQuestionnaire ->
                            ( "fa-globe", "Public" )

                        PrivateQuestionnaire ->
                            ( "fa-lock", "Private" )

                        VisibleViewQuestionnaire ->
                            ( "fa-shield-alt", "Public Read-Only" )
            in
            span [ class "fragment" ]
                [ i [ class ("fa fas " ++ icon), title iconTitle ] [] ]

        ownerBadge questionnaire =
            if Maybe.map .uuid questionnaire.owner == Maybe.map .uuid appState.session.user then
                Badge.outline appState.theme [] [ text "Owner" ]

            else
                emptyNode

        viewTitle questionnaire =
            span []
                [ a [ class "fragment link", href <| Route.toString (Route.Project questionnaire.uuid ProjectRoute.Overview) ]
                    [ text questionnaire.name
                    ]
                , visibilityIcon questionnaire
                , ownerBadge questionnaire
                ]

        toAnsweredInidcation answeredInidciation =
            let
                { answeredQuestions, unansweredQuestions } =
                    SummaryReport.unwrapIndicationReport answeredInidciation

                value =
                    toFloat answeredQuestions / (toFloat answeredQuestions + toFloat unansweredQuestions)
            in
            span []
                [ span [ css [ width (px 100), display inlineBlock, Spacing.inlineSM ] ] [ ProgressBar.default appState.theme value [] ]
                , text (String.fromInt answeredQuestions ++ "/" ++ String.fromInt (answeredQuestions + unansweredQuestions))
                ]

        answered questionnaire =
            questionnaire.report.indications
                |> List.sortWith SummaryReport.compareIndicationReport
                |> List.take 1
                |> List.map toAnsweredInidcation

        levelName questionnaire =
            let
                levelNameValue =
                    List.find (\l -> l.level == questionnaire.level) levels
                        |> Maybe.unwrap "" .title
            in
            span [ css [ Spacing.inlineMD ] ] [ text levelNameValue ]

        viewDescription questionnaire =
            span []
                (levelName questionnaire :: answered questionnaire)

        viewConfig =
            { title = viewTitle
            , description = viewDescription
            , textTitle = .name
            , emptyText = "No questionnaires"
            , sortOptions =
                [ ( "name", "Name" )
                , ( "createdAt", "Created at" )
                , ( "updatedAt", "Updated at" )
                ]
            , wrapMsg = ListingMsg
            , toRoute = Route.toString << Route.Dashboard
            , updated =
                Just
                    { getTime = .updatedAt
                    , currentTime = appState.currentTime
                    }
            }
    in
    Grid.comfortable.container
        [ Grid.containerLimitedSmall, Grid.containerIndented ]
        [ Grid.comfortable.row []
            [ Grid.comfortable.col 6
                []
                [ h1 [] [ text "Projects" ]
                ]
            , Grid.comfortable.col 6
                [ Grid.colTextRight ]
                [ Button.primaryLink appState.theme
                    [ href (Route.toString Route.ProjectCreate) ]
                    [ fa "fas fa-plus"
                    , span [] [ text "Create project" ]
                    ]
                ]
            ]
        , Grid.comfortable.row []
            [ Grid.comfortable.col 12
                []
                [ Listing.view appState.theme viewConfig model.questionnaires
                ]
            ]
        ]
