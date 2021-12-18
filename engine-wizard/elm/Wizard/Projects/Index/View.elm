module Wizard.Projects.Index.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Dict
import Html exposing (Html, code, div, img, input, span, text)
import Html.Attributes exposing (class, classList, href, placeholder, src, title, type_, value)
import Html.Events exposing (onInput)
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Data.Pagination as Pagination
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Shared.Data.Questionnaire.QuestionnaireState exposing (QuestionnaireState(..))
import Shared.Data.SummaryReport exposing (IndicationReport(..), compareIndicationReport, unwrapIndicationReport)
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lgx, lx)
import Shared.Utils exposing (flip, listFilterJust, listInsertIf)
import Uuid
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Feature as Features
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, listClass)
import Wizard.Common.Html.Events exposing (alwaysStopPropagationOn)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.KnowledgeModels.Routes
import Wizard.Projects.Common.CloneProjectModal.Msgs as CloneProjectModalMsg
import Wizard.Projects.Common.CloneProjectModal.View as CloneProjectModal
import Wizard.Projects.Common.DeleteProjectModal.Msgs as DeleteProjectModalMsg
import Wizard.Projects.Common.DeleteProjectModal.View as DeleteProjectModal
import Wizard.Projects.Common.QuestionnaireDescriptor as QuestionnaireDescriptor
import Wizard.Projects.Common.View exposing (visibilityIcons)
import Wizard.Projects.Create.ProjectCreateRoute exposing (ProjectCreateRoute(..))
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute exposing (ProjectDetailRoute(..))
import Wizard.Projects.Index.Models exposing (Model)
import Wizard.Projects.Index.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..), indexRouteIsTemplateFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)
import Wizard.Routes as Routes
import Wizard.Routing as Routing


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        userFilterSelectedUsersActionResult =
            if Dict.member indexRouteUsersFilterId model.questionnaires.filters then
                model.userFilterSelectedUsers

            else
                ActionResult.Success Pagination.empty

        actionResult =
            ActionResult.combine3 model.projectTagsFilterTags model.userFilterUsers userFilterSelectedUsersActionResult

        content _ =
            div [ listClass "Questionnaires__Index" ]
                [ Page.header (l_ "header.title" appState) []
                , FormResult.successOnlyView appState model.deleteModalModel.deletingQuestionnaire
                , FormResult.view appState model.deletingMigration
                , Listing.view appState (listingConfig appState model) model.questionnaires
                , Html.map DeleteQuestionnaireModalMsg <| DeleteProjectModal.view appState model.deleteModalModel
                , Html.map CloneQuestionnaireModalMsg <| CloneProjectModal.view appState model.cloneModalModel
                ]
    in
    Page.actionResultView appState content actionResult


createButton : AppState -> Html Msg
createButton appState =
    let
        createRoute =
            CreateRoute <|
                if QuestionnaireCreation.fromTemplateEnabled appState.config.questionnaire.questionnaireCreation then
                    TemplateCreateRoute Nothing

                else
                    CustomCreateRoute Nothing
    in
    linkTo appState
        (Routes.ProjectsRoute createRoute)
        [ class "btn btn-primary", dataCy "projects_create-button" ]
        [ lx_ "header.create" appState ]


listingConfig : AppState -> Model -> ViewConfig Questionnaire Msg
listingConfig appState model =
    let
        templateFilter =
            Listing.SimpleFilter indexRouteIsTemplateFilterId
                { name = l_ "filter.template.name" appState
                , options =
                    [ ( "true", l_ "filter.template.templatesOnly" appState )
                    , ( "false", l_ "filter.template.projectsOnly" appState )
                    ]
                }

        tagsFilter =
            listingProjectTagsFilter appState model

        tagsFilterVisible =
            Dict.member indexRouteProjectTagsFilterId model.questionnaires.filters
                || ActionResult.unwrap False (not << List.isEmpty << .items) model.projectTagsFilterTags

        usersFilter =
            listingUsersFilter appState model

        listingFilters =
            []
                |> listInsertIf templateFilter (Features.projectTemplatesCreate appState)
                |> listInsertIf tagsFilter (Features.projectTagging appState && tagsFilterVisible)
                |> listInsertIf usersFilter True
    in
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
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
    , searchPlaceholderText = Just (l_ "listing.searchPlaceholderText" appState)
    , sortOptions =
        [ ( "name", lg "questionnaire.name" appState )
        , ( "createdAt", lg "questionnaire.createdAt" appState )
        , ( "updatedAt", lg "questionnaire.updatedAt" appState )
        ]
    , filters = listingFilters
    , toRoute = Routes.projectIndexWithFilters
    , toolbarExtra = Just (createButton appState)
    }


listingProjectTagsFilter : AppState -> Model -> Listing.Filter Msg
listingProjectTagsFilter appState model =
    let
        toRoute tags =
            Routing.toUrl appState <|
                Routes.projectIndexWithFilters
                    (Dict.insert indexRouteProjectTagsFilterId (String.join "," (List.unique tags)) model.questionnaires.filters)
                    model.questionnaires.paginationQueryString

        removeTagLink tag =
            toRoute <| List.filter ((/=) tag) selectedTags

        addTagLink tag =
            toRoute <| tag :: selectedTags

        viewTagItem link icon tag =
            Dropdown.anchorItem
                [ href (link tag), class "dropdown-item-icon", dataCy "project_filter_tags_option" ]
                [ icon, text tag ]

        selectedTagItem =
            viewTagItem removeTagLink (faSet "listing.filter.multi.selected" appState)

        addTagItem =
            viewTagItem addTagLink (faSet "listing.filter.multi.notSelected" appState)

        sortTags =
            List.sortBy String.toUpper

        selectedTags =
            model.questionnaires.filters
                |> Dict.get indexRouteProjectTagsFilterId
                |> Maybe.unwrap [] (sortTags << String.split ",")

        foundTags =
            model.projectTagsFilterTags
                |> ActionResult.unwrap [] (sortTags << .items)

        badge =
            filterBadge selectedTags

        searchInputItem =
            [ Dropdown.customItem <|
                div [ class "dropdown-item-search" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , placeholder (l_ "filter.projectTags.searchPlaceholder" appState)
                        , alwaysStopPropagationOn "click" (D.succeed (ProjectTagsFilterInput model.projectTagsFilterSearchValue))
                        , onInput ProjectTagsFilterInput
                        , value model.projectTagsFilterSearchValue
                        ]
                        []
                    ]
            , Dropdown.divider
            ]

        selectedTagsItems =
            List.map selectedTagItem selectedTags

        foundTagsItems =
            List.map addTagItem foundTags

        label =
            case List.head selectedTags of
                Just selectedTag ->
                    selectedTag

                Nothing ->
                    l_ "filter.projectTags.title" appState
    in
    Listing.CustomFilter indexRouteProjectTagsFilterId
        { label = [ span [ class "filter-text-label" ] [ text label ], badge ]
        , items = searchInputItem ++ selectedTagsItems ++ foundTagsItems
        }


listingUsersFilter : AppState -> Model -> Listing.Filter Msg
listingUsersFilter appState model =
    let
        toRoute userUuids =
            Routing.toUrl appState <|
                Routes.projectIndexWithFilters
                    (Dict.insert indexRouteUsersFilterId (String.join "," (List.unique userUuids)) model.questionnaires.filters)
                    model.questionnaires.paginationQueryString

        removeUserLink userUuid =
            toRoute <| List.filter ((/=) (Uuid.toString userUuid)) selectedUserUuidss

        addUserLink userUuid =
            toRoute <| Uuid.toString userUuid :: selectedUserUuidss

        viewUserItem link icon user =
            Dropdown.anchorItem
                [ href (link user.uuid), class "dropdown-item-icon", dataCy "project_filter_users_option" ]
                [ icon
                , UserIcon.viewSmall user
                , text (User.fullName user)
                ]

        selectedUserItem =
            viewUserItem removeUserLink (faSet "listing.filter.multi.selected" appState)

        addUserItem =
            viewUserItem addUserLink (faSet "listing.filter.multi.notSelected" appState)

        foundSelectedUsers =
            ActionResult.unwrap [] .items model.userFilterSelectedUsers
                |> List.sortWith User.compare

        selectedUserUuidss =
            model.questionnaires.filters
                |> Dict.get indexRouteUsersFilterId
                |> Maybe.unwrap [] (String.split ",")

        selectedUsers =
            selectedUserUuidss
                |> List.map (\a -> List.find (\u -> Uuid.toString u.uuid == a) foundSelectedUsers)
                |> listFilterJust
                |> List.sortWith User.compare

        foundUsers =
            model.userFilterUsers
                |> ActionResult.unwrap [] (List.sortWith User.compare << .items)

        badge =
            filterBadge selectedUsers

        searchInputItem =
            [ Dropdown.customItem <|
                div [ class "dropdown-item-search" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , placeholder (l_ "filter.users.searchPlaceholder" appState)
                        , alwaysStopPropagationOn "click" (D.succeed (UsersFilterInput model.userFilterSearchValue))
                        , onInput UsersFilterInput
                        , value model.userFilterSearchValue
                        ]
                        []
                    ]
            , Dropdown.divider
            ]

        selectedUsersItems =
            List.map selectedUserItem selectedUsers

        foundUsersItems =
            List.map addUserItem foundUsers

        label =
            case List.head selectedUsers of
                Just selectedUser ->
                    User.fullName selectedUser

                Nothing ->
                    l_ "filter.users.title" appState
    in
    Listing.CustomFilter indexRouteUsersFilterId
        { label = [ span [ class "filter-text-label" ] [ text label ], badge ]
        , items = searchInputItem ++ selectedUsersItems ++ foundUsersItems
        }


filterBadge : List a -> Html msg
filterBadge items =
    case List.length items of
        0 ->
            emptyNode

        1 ->
            emptyNode

        n ->
            span [ class "badge badge-pill badge-dark" ] [ text ("+" ++ String.fromInt (n - 1)) ]


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
        (linkTo appState (linkRoute questionnaire) [] [ text questionnaire.name ]
            :: templateBadge appState questionnaire
            :: visibilityIcons appState questionnaire
            ++ [ stateBadge appState questionnaire ]
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
                , dataCy = "open"
                }

        openProjectVisible =
            Features.projectOpen appState questionnaire

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
                , dataCy = "clone"
                }

        cloneVisible =
            Features.projectClone appState questionnaire

        createMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createMigration" appState
                , label = l_ "action.createMigration" appState
                , msg = ListingActionLink (Routes.ProjectsRoute <| CreateMigrationRoute questionnaire.uuid)
                , dataCy = "create-migration"
                }

        createMigrationVisible =
            Features.projectCreateMigration appState questionnaire

        continueMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createMigration" appState
                , label = l_ "action.continueMigration" appState
                , msg = ListingActionLink (Routes.ProjectsRoute <| MigrationRoute questionnaire.uuid)
                , dataCy = "continue-migration"
                }

        continueMigrationVisible =
            Features.projectContinueMigration appState questionnaire

        cancelMigration =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.cancel" appState
                , label = l_ "action.cancelMigration" appState
                , msg = ListingActionMsg (DeleteQuestionnaireMigration questionnaire.uuid)
                , dataCy = "cancel-migration"
                }

        cancelMigrationVisible =
            Features.projectCancelMigration appState questionnaire

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
                , dataCy = "delete"
                }

        deleteVisible =
            Features.projectDelete appState questionnaire
    in
    []
        |> listInsertIf openProject openProjectVisible
        |> listInsertIf Listing.dropdownSeparator (cloneVisible || continueMigrationVisible || cancelMigrationVisible || createMigrationVisible)
        |> listInsertIf clone cloneVisible
        |> listInsertIf continueMigration continueMigrationVisible
        |> listInsertIf cancelMigration cancelMigrationVisible
        |> listInsertIf createMigration createMigrationVisible
        |> listInsertIf Listing.dropdownSeparator deleteVisible
        |> listInsertIf delete deleteVisible


detailRoute : Questionnaire -> Routes.Route
detailRoute =
    Routes.ProjectsRoute << flip Wizard.Projects.Routes.DetailRoute PlanDetailRoute.Questionnaire << .uuid


migrationRoute : Questionnaire -> Routes.Route
migrationRoute =
    Routes.ProjectsRoute << MigrationRoute << .uuid


stateBadge : AppState -> Questionnaire -> Html msg
stateBadge appState questionnaire =
    case questionnaire.state of
        Migrating ->
            span [ class "badge badge-info" ]
                [ lx_ "badge.migrating" appState ]

        Outdated ->
            linkTo appState
                (Routes.ProjectsRoute <| CreateMigrationRoute questionnaire.uuid)
                [ class "badge badge-warning" ]
                [ lx_ "badge.outdated" appState ]

        Default ->
            emptyNode


templateBadge : AppState -> Questionnaire -> Html msg
templateBadge appState questionnaire =
    if questionnaire.isTemplate then
        span [ class "badge badge-info" ]
            [ lgx "questionnaire.templateBadge" appState ]

    else
        emptyNode
