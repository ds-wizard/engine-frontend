module Wizard.Projects.Index.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Data.Questionnaire.QuestionnaireState exposing (QuestionnaireState(..))
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Shared.Data.SummaryReport exposing (IndicationReport(..), compareIndicationReport, unwrapIndicationReport)
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lh, lx)
import Shared.Utils exposing (flip, listInsertIf)
import Version exposing (Version)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionConfig, ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Routes
import Wizard.Projects.Common.CloneProjectModal.Msgs as CloneProjectModalMsg
import Wizard.Projects.Common.CloneProjectModal.View as CloneProjectModal
import Wizard.Projects.Common.DeleteProjectModal.Msgs as DeleteProjectModalMsg
import Wizard.Projects.Common.DeleteProjectModal.View as DeleteProjectModal
import Wizard.Projects.Common.QuestionnaireDescriptor as QuestionnaireDescriptor
import Wizard.Projects.Common.View exposing (visibilityIcons)
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute exposing (ProjectDetailRoute(..))
import Wizard.Projects.Index.Models exposing (Model)
import Wizard.Projects.Index.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Index.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Projects.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "Questionnaires__Index" ]
        [ Page.header (l_ "header.title" appState) []
        , FormResult.successOnlyView appState model.deleteModalModel.deletingQuestionnaire
        , FormResult.view appState model.deletingMigration
        , Listing.view appState (listingConfig appState) model.questionnaires
        , Html.map DeleteQuestionnaireModalMsg <| DeleteProjectModal.view appState model.deleteModalModel
        , Html.map CloneQuestionnaireModalMsg <| CloneProjectModal.view appState model.cloneModalModel
        ]


createButton : AppState -> Html Msg
createButton appState =
    linkTo appState
        (Routes.ProjectsRoute <| CreateRoute Nothing)
        [ class "btn btn-primary" ]
        [ lx_ "header.create" appState ]


listingConfig : AppState -> ViewConfig Questionnaire Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = l_ "listing.empty" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , sortOptions =
        [ ( "name", lg "questionnaire.name" appState )
        , ( "createdAt", lg "questionnaire.createdAt" appState )
        , ( "updatedAt", lg "questionnaire.updatedAt" appState )
        ]
    , toRoute = Routes.ProjectsRoute << IndexRoute
    , toolbarExtra = Just (createButton appState)
    }


listingTitle : AppState -> Questionnaire -> Html Msg
listingTitle appState questionnaire =
    let
        linkRoute =
            if questionnaire.state == Migrating then
                migrationRoute

            else
                detailRoute
    in
    span []
        ([ linkTo appState (linkRoute questionnaire) [] [ text questionnaire.name ] ]
            ++ visibilityIcons appState questionnaire
            ++ [ stateBadge appState questionnaire.state ]
        )


listingDescription : AppState -> Questionnaire -> Html Msg
listingDescription appState questionnaire =
    let
        collaborators =
            case questionnaire.permissions of
                [] ->
                    emptyNode

                perm :: [] ->
                    span [ class "fragment" ]
                        [ img [ src (User.imageUrlOrGravatar perm.member), class "user-icon user-icon-small" ] []
                        , text <| User.fullName perm.member
                        ]

                perms ->
                    let
                        ownerIcon member =
                            img
                                [ src (User.imageUrlOrGravatar member)
                                , class "user-icon user-icon-small user-icon-only"
                                , title <| User.fullName member
                                ]
                                []

                        users =
                            perms
                                |> List.map .member
                                |> List.sortWith User.compare
                                |> List.take 5
                                |> List.map ownerIcon

                        extraUsers =
                            if List.length perms > 5 then
                                span [] [ text ("+" ++ String.fromInt (List.length perms - 5)) ]

                            else
                                emptyNode
                    in
                    span [ class "fragment" ] (users ++ [ extraUsers ])

        kmRoute =
            Routes.KnowledgeModelsRoute <|
                Wizard.KnowledgeModels.Routes.DetailRoute questionnaire.package.id

        kmLink =
            linkTo appState
                kmRoute
                [ title <| lg "knowledgeModel" appState, class "fragment" ]
                [ text questionnaire.package.name
                , text ", "
                , text <| Version.toString questionnaire.package.version
                , text " ("
                , code [] [ text questionnaire.package.id ]
                , text ")"
                ]

        toAnsweredInidcation answeredInidciation =
            let
                { answeredQuestions, unansweredQuestions } =
                    unwrapIndicationReport answeredInidciation
            in
            span [ class "fragment", classList [ ( "text-success", unansweredQuestions == 0 ) ] ]
                [ text ("Answered " ++ String.fromInt answeredQuestions ++ "/" ++ String.fromInt (answeredQuestions + unansweredQuestions)) ]

        answered =
            questionnaire.report.indications
                |> List.sortWith compareIndicationReport
                |> List.take 1
                |> List.map toAnsweredInidcation
    in
    span []
        (collaborators :: kmLink :: answered)


listingActions : AppState -> Questionnaire -> List (ListingDropdownItem Msg)
listingActions appState questionnaire =
    let
        openProject =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "project.open" appState
                , label = l_ "action.open" appState
                , msg = ListingActionLink (detailRoute questionnaire)
                }

        clone =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.clone" appState
                , label = l_ "action.clone" appState
                , msg =
                    QuestionnaireDescriptor.fromQuestionnaire questionnaire
                        |> Just
                        |> CloneProjectModalMsg.ShowHideCloneQuestionnaire
                        |> CloneQuestionnaireModalMsg
                        |> ListingActionMsg
                }

        createMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createMigration" appState
                , label = l_ "action.createMigration" appState
                , msg = ListingActionLink (Routes.ProjectsRoute <| CreateMigrationRoute questionnaire.uuid)
                }

        continueMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createMigration" appState
                , label = l_ "action.continueMigration" appState
                , msg = ListingActionLink (Routes.ProjectsRoute <| MigrationRoute questionnaire.uuid)
                }

        cancelMigration =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.cancel" appState
                , label = l_ "action.cancelMigration" appState
                , msg = ListingActionMsg (DeleteQuestionnaireMigration questionnaire.uuid)
                }

        delete =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = l_ "action.delete" appState
                , msg =
                    QuestionnaireDescriptor.fromQuestionnaire questionnaire
                        |> Just
                        |> DeleteProjectModalMsg.ShowHideDeleteQuestionnaire
                        |> DeleteQuestionnaireModalMsg
                        |> ListingActionMsg
                }

        editable =
            Questionnaire.isEditable appState questionnaire

        migrating =
            questionnaire.state == Migrating
    in
    []
        |> listInsertIf openProject (not migrating)
        |> listInsertIf Listing.dropdownSeparator (not migrating)
        |> listInsertIf clone (not migrating)
        |> listInsertIf continueMigration (editable && migrating)
        |> listInsertIf cancelMigration (editable && migrating)
        |> listInsertIf createMigration (editable && not migrating)
        |> listInsertIf Listing.dropdownSeparator editable
        |> listInsertIf delete editable


detailRoute : Questionnaire -> Routes.Route
detailRoute =
    Routes.ProjectsRoute << flip Wizard.Projects.Routes.DetailRoute PlanDetailRoute.Questionnaire << .uuid


migrationRoute : Questionnaire -> Routes.Route
migrationRoute =
    Routes.ProjectsRoute << MigrationRoute << .uuid


stateBadge : AppState -> QuestionnaireState -> Html msg
stateBadge appState state =
    case state of
        Migrating ->
            span [ class "badge badge-info" ]
                [ lx_ "badge.migrating" appState ]

        Outdated ->
            span [ class "badge badge-warning" ]
                [ lx_ "badge.outdated" appState ]

        Default ->
            emptyNode
