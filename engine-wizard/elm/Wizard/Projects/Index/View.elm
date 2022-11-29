module Wizard.Projects.Index.View exposing (view)

import ActionResult
import Bootstrap.Dropdown as Dropdown
import Gettext exposing (gettext)
import Html exposing (Html, a, code, div, img, input, span, text)
import Html.Attributes exposing (class, classList, href, placeholder, src, style, title, type_, value)
import Html.Events exposing (onInput)
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Components.Badge as Badge
import Shared.Data.PackageSuggestion as PackageSuggestion
import Shared.Data.Pagination as Pagination
import Shared.Data.PaginationQueryFilters as PaginationQueryFilter
import Shared.Data.PaginationQueryFilters.FilterOperator as FilterOperator
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.Questionnaire.QuestionnaireState exposing (QuestionnaireState(..))
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, faSet)
import Shared.Utils exposing (listFilterJust, listInsertIf)
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
import Wizard.Projects.Index.Models exposing (Model)
import Wizard.Projects.Index.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..), indexRouteIsTemplateFilterId, indexRoutePackagesFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)
import Wizard.Routes as Routes
import Wizard.Routing as Routing


view : AppState -> Model -> Html Msg
view appState model =
    let
        userFilterSelectedUsersActionResult =
            if PaginationQueryFilter.isFilterActive indexRouteUsersFilterId model.questionnaires.filters then
                model.userFilterSelectedUsers

            else
                ActionResult.Success Pagination.empty

        actionResult =
            ActionResult.combine3 model.projectTagsFilterTags model.userFilterUsers userFilterSelectedUsersActionResult

        content _ =
            div [ listClass "Questionnaires__Index" ]
                [ Page.header (gettext "Projects" appState.locale) []
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
    linkTo appState
        (Routes.projectsCreate appState)
        [ class "btn btn-primary", dataCy "projects_create-button" ]
        [ text (gettext "Create" appState.locale) ]


listingConfig : AppState -> Model -> ViewConfig Questionnaire Msg
listingConfig appState model =
    let
        templateFilter =
            Listing.SimpleFilter indexRouteIsTemplateFilterId
                { name = gettext "Project Template" appState.locale
                , options =
                    [ ( "true", gettext "Templates only" appState.locale )
                    , ( "false", gettext "Projects only" appState.locale )
                    ]
                }

        tagsFilter =
            listingProjectTagsFilter appState model

        tagsFilterVisible =
            PaginationQueryFilter.isFilterActive indexRouteProjectTagsFilterId model.questionnaires.filters
                || ActionResult.withDefault False model.projectTagsExist

        kmsFilter =
            listingKMsFilter appState model

        usersFilter =
            listingUsersFilter appState model

        listingFilters =
            []
                |> listInsertIf templateFilter (Features.projectTemplatesCreate appState)
                |> listInsertIf tagsFilter (Features.projectTagging appState && tagsFilterVisible)
                |> listInsertIf kmsFilter True
                |> listInsertIf usersFilter True
    in
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = gettext "Click \"Create\" button to add a new project." appState.locale
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search projects..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        , ( "updatedAt", gettext "Updated" appState.locale )
        ]
    , filters = listingFilters
    , toRoute = Routes.projectsIndexWithFilters
    , toolbarExtra = Just (createButton appState)
    }


listingProjectTagsFilter : AppState -> Model -> Listing.Filter Msg
listingProjectTagsFilter appState model =
    let
        linkWithTags tags =
            Routing.toUrl appState <|
                Routes.projectsIndexWithFilters
                    (PaginationQueryFilter.insertValue indexRouteProjectTagsFilterId (String.join "," (List.unique tags)) model.questionnaires.filters)
                    (PaginationQueryString.resetPage model.questionnaires.paginationQueryString)

        linkWithOp op =
            Routing.toUrl appState <|
                Routes.projectsIndexWithFilters
                    (PaginationQueryFilter.insertOp indexRouteProjectTagsFilterId op model.questionnaires.filters)
                    (PaginationQueryString.resetPage model.questionnaires.paginationQueryString)

        removeTagLink tag =
            linkWithTags <| List.filter ((/=) tag) selectedTags

        addTagLink tag =
            linkWithTags <| tag :: selectedTags

        viewTagItem link icon tag =
            Dropdown.anchorItem
                [ href (link tag)
                , class "dropdown-item-icon"
                , dataCy "project_filter_tags_option"
                , alwaysStopPropagationOn "click" (D.succeed NoOp)
                ]
                [ icon, text tag ]

        selectedTagItem =
            viewTagItem removeTagLink (faSet "listing.filter.multi.selected" appState)

        sortTags =
            List.sortBy String.toUpper

        selectedTags =
            model.questionnaires.filters
                |> PaginationQueryFilter.getValue indexRouteProjectTagsFilterId
                |> Maybe.unwrap [] (sortTags << String.split ",")

        foundTags =
            model.projectTagsFilterTags
                |> ActionResult.unwrap [] (sortTags << .items)

        badge =
            filterBadge selectedTags

        filterOperator =
            Maybe.withDefault FilterOperator.OR <| PaginationQueryFilter.getOp indexRouteProjectTagsFilterId model.questionnaires.filters

        searchInputItem =
            [ Dropdown.customItem <|
                div [ class "dropdown-item-search" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , placeholder (gettext "Search project tags..." appState.locale)
                        , alwaysStopPropagationOn "click" (D.succeed (ProjectTagsFilterInput model.projectTagsFilterSearchValue))
                        , onInput ProjectTagsFilterInput
                        , value model.projectTagsFilterSearchValue
                        ]
                        []
                    ]
            , Dropdown.divider
            , Dropdown.customItem <|
                div [ class "dropdown-item-operator" ]
                    [ a
                        [ href (linkWithOp FilterOperator.OR)
                        , classList [ ( "active", filterOperator == FilterOperator.OR ) ]
                        , dataCy "filter_projectTags_operator_OR"
                        , alwaysStopPropagationOn "click" (D.succeed NoOp)
                        ]
                        [ text (gettext "OR" appState.locale) ]
                    , a
                        [ href (linkWithOp FilterOperator.AND)
                        , classList [ ( "active", filterOperator == FilterOperator.AND ) ]
                        , dataCy "filter_projectTags_operator_AND"
                        , alwaysStopPropagationOn "click" (D.succeed NoOp)
                        ]
                        [ text (gettext "AND" appState.locale) ]
                    ]
            , Dropdown.divider
            ]

        selectedTagsItems =
            List.map selectedTagItem selectedTags

        foundTagsItems =
            if not (List.isEmpty foundTags) then
                let
                    addTagItem =
                        viewTagItem addTagLink (faSet "listing.filter.multi.notSelected" appState)
                in
                List.map addTagItem foundTags

            else if not (String.isEmpty model.projectTagsFilterSearchValue) then
                [ Dropdown.customItem <|
                    div [ class "dropdown-item-empty" ]
                        [ text (gettext "No project tags found" appState.locale) ]
                ]

            else
                []

        label =
            case List.head selectedTags of
                Just selectedTag ->
                    selectedTag

                Nothing ->
                    gettext "Project Tags" appState.locale
    in
    Listing.CustomFilter indexRouteProjectTagsFilterId
        { label = [ span [ class "filter-text-label" ] [ text label ], badge ]
        , items = searchInputItem ++ selectedTagsItems ++ foundTagsItems
        }


listingKMsFilter : AppState -> Model -> Listing.Filter Msg
listingKMsFilter appState model =
    let
        linkWithIds packageIds =
            Routing.toUrl appState <|
                Routes.projectsIndexWithFilters
                    (PaginationQueryFilter.insertValue indexRoutePackagesFilterId (String.join "," (List.unique packageIds)) model.questionnaires.filters)
                    (PaginationQueryString.resetPage model.questionnaires.paginationQueryString)

        removePackageLink packageId =
            linkWithIds <| List.filter ((/=) (PackageSuggestion.packageIdAll packageId)) selectedPackageIds

        addPackageLink packageId =
            linkWithIds <| PackageSuggestion.packageIdAll packageId :: selectedPackageIds

        viewPackageItem link icon package =
            Dropdown.anchorItem
                [ href (link package.id)
                , class "dropdown-item-icon"
                , dataCy "project_filter_packages_option"
                , alwaysStopPropagationOn "click" (D.succeed NoOp)
                ]
                [ icon
                , text package.name
                ]

        selectedPackageItem =
            viewPackageItem removePackageLink (faSet "listing.filter.multi.selected" appState)

        foundSelectedPackages =
            ActionResult.unwrap [] .items model.packagesFilterSelectedPackages
                |> List.sortBy .name

        selectedPackageIds =
            model.questionnaires.filters
                |> PaginationQueryFilter.getValue indexRoutePackagesFilterId
                |> Maybe.unwrap [] (String.split ",")

        selectedPackages =
            selectedPackageIds
                |> List.map (\a -> List.find (PackageSuggestion.isSamePackage a << .id) foundSelectedPackages)
                |> listFilterJust
                |> List.sortBy .name

        foundPackages =
            model.packagesFilterPackages
                |> ActionResult.unwrap [] (List.sortBy .name << .items)

        badge =
            filterBadge selectedPackages

        searchInputItem =
            [ Dropdown.customItem <|
                div [ class "dropdown-item-search" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , placeholder (gettext "Search knowledge models..." appState.locale)
                        , alwaysStopPropagationOn "click" (D.succeed (PackagesFilterInput model.packagesFilterSearchValue))
                        , onInput PackagesFilterInput
                        , value model.packagesFilterSearchValue
                        ]
                        []
                    ]
            , Dropdown.divider
            ]

        selectedPackagesItems =
            List.map selectedPackageItem selectedPackages

        foundPackagesItems =
            if not (List.isEmpty foundPackages) then
                let
                    addPackageItem =
                        viewPackageItem addPackageLink (faSet "listing.filter.multi.notSelected" appState)
                in
                List.map addPackageItem foundPackages

            else if not (String.isEmpty model.packagesFilterSearchValue) then
                [ Dropdown.customItem <|
                    div [ class "dropdown-item-empty" ]
                        [ text (gettext "No knowledge models found" appState.locale) ]
                ]

            else
                []

        label =
            case List.head selectedPackages of
                Just selectedPackage ->
                    selectedPackage.name

                Nothing ->
                    gettext "Knowledge Models" appState.locale
    in
    Listing.CustomFilter indexRoutePackagesFilterId
        { label = [ span [ class "filter-text-label" ] [ text label ], badge ]
        , items = searchInputItem ++ selectedPackagesItems ++ foundPackagesItems
        }


listingUsersFilter : AppState -> Model -> Listing.Filter Msg
listingUsersFilter appState model =
    let
        linkWithUuids userUuids =
            Routing.toUrl appState <|
                Routes.projectsIndexWithFilters
                    (PaginationQueryFilter.insertValue indexRouteUsersFilterId (String.join "," (List.unique userUuids)) model.questionnaires.filters)
                    (PaginationQueryString.resetPage model.questionnaires.paginationQueryString)

        linkWithOp op =
            Routing.toUrl appState <|
                Routes.projectsIndexWithFilters
                    (PaginationQueryFilter.insertOp indexRouteUsersFilterId op model.questionnaires.filters)
                    (PaginationQueryString.resetPage model.questionnaires.paginationQueryString)

        removeUserLink userUuid =
            linkWithUuids <| List.filter ((/=) (Uuid.toString userUuid)) selectedUserUuids

        addUserLink userUuid =
            linkWithUuids <| Uuid.toString userUuid :: selectedUserUuids

        viewUserItem link icon user =
            Dropdown.anchorItem
                [ href (link user.uuid)
                , class "dropdown-item-icon"
                , dataCy "project_filter_users_option"
                , alwaysStopPropagationOn "click" (D.succeed NoOp)
                ]
                [ icon
                , UserIcon.viewSmall user
                , text (User.fullName user)
                ]

        selectedUserItem =
            viewUserItem removeUserLink (faSet "listing.filter.multi.selected" appState)

        foundSelectedUsers =
            ActionResult.unwrap [] .items model.userFilterSelectedUsers
                |> List.sortWith User.compare

        selectedUserUuids =
            model.questionnaires.filters
                |> PaginationQueryFilter.getValue indexRouteUsersFilterId
                |> Maybe.unwrap [] (String.split ",")

        selectedUsers =
            selectedUserUuids
                |> List.map (\a -> List.find (\u -> Uuid.toString u.uuid == a) foundSelectedUsers)
                |> listFilterJust
                |> List.sortWith User.compare

        foundUsers =
            model.userFilterUsers
                |> ActionResult.unwrap [] (List.sortWith User.compare << .items)

        badge =
            filterBadge selectedUsers

        filterOperator =
            Maybe.withDefault FilterOperator.OR <| PaginationQueryFilter.getOp indexRouteUsersFilterId model.questionnaires.filters

        searchInputItem =
            [ Dropdown.customItem <|
                div [ class "dropdown-item-search" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , placeholder (gettext "Search users..." appState.locale)
                        , alwaysStopPropagationOn "click" (D.succeed (UsersFilterInput model.userFilterSearchValue))
                        , onInput UsersFilterInput
                        , value model.userFilterSearchValue
                        ]
                        []
                    ]
            , Dropdown.divider
            , Dropdown.customItem <|
                div [ class "dropdown-item-operator" ]
                    [ a
                        [ href (linkWithOp FilterOperator.OR)
                        , classList [ ( "active", filterOperator == FilterOperator.OR ) ]
                        , dataCy "filter_users_operator_OR"
                        , alwaysStopPropagationOn "click" (D.succeed NoOp)
                        ]
                        [ text (gettext "OR" appState.locale) ]
                    , a
                        [ href (linkWithOp FilterOperator.AND)
                        , classList [ ( "active", filterOperator == FilterOperator.AND ) ]
                        , dataCy "filter_users_operator_AND"
                        , alwaysStopPropagationOn "click" (D.succeed NoOp)
                        ]
                        [ text (gettext "AND" appState.locale) ]
                    ]
            , Dropdown.divider
            ]

        selectedUsersItems =
            List.map selectedUserItem selectedUsers

        foundUsersItems =
            if not (List.isEmpty foundUsers) then
                let
                    addUserItem =
                        viewUserItem addUserLink (faSet "listing.filter.multi.notSelected" appState)
                in
                List.map addUserItem foundUsers

            else if not (String.isEmpty model.userFilterSearchValue) then
                [ Dropdown.customItem <|
                    div [ class "dropdown-item-empty" ]
                        [ text (gettext "No users found" appState.locale) ]
                ]

            else
                []

        label =
            case List.head selectedUsers of
                Just selectedUser ->
                    User.fullName selectedUser

                Nothing ->
                    gettext "Users" appState.locale
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
            Badge.dark [ class "rounded-pill" ] [ text ("+" ++ String.fromInt (n - 1)) ]


listingTitle : AppState -> Questionnaire -> Html Msg
listingTitle appState questionnaire =
    let
        linkRoute =
            if questionnaire.state == Migrating then
                Routes.projectsMigration

            else
                Routes.projectsDetailQuestionnaire
    in
    span []
        (linkTo appState (linkRoute questionnaire.uuid) [] [ text questionnaire.name ]
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
                [ title <| gettext "Knowledge Model" appState.locale, class "fragment" ]
                [ text questionnaire.package.name
                , text ", "
                , text <| Version.toString questionnaire.package.version
                , text " ("
                , code [] [ text questionnaire.package.id ]
                , text ")"
                ]

        projectProgressView =
            if questionnaire.answeredQuestions == 0 && questionnaire.unansweredQuestions == 0 then
                emptyNode

            else
                let
                    pctg =
                        (toFloat questionnaire.answeredQuestions / toFloat (questionnaire.answeredQuestions + questionnaire.unansweredQuestions)) * 100
                in
                span [ class "fragment flex-grow-1" ]
                    [ span [ class "progress", style "height" "7px" ]
                        [ span [ class "progress-bar bg-info", style "width" (String.fromFloat pctg ++ "%") ] [] ]
                    ]
    in
    span []
        [ collaborators, kmLink, projectProgressView ]


listingActions : AppState -> Questionnaire -> List (ListingDropdownItem Msg)
listingActions appState questionnaire =
    let
        openProject =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "project.open" appState
                , label = gettext "Open project" appState.locale
                , msg = ListingActionLink (Routes.projectsDetailQuestionnaire questionnaire.uuid)
                , dataCy = "open"
                }

        openProjectVisible =
            Features.projectOpen appState questionnaire

        clone =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.clone" appState
                , label = gettext "Clone" appState.locale
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
                , label = gettext "Create migration" appState.locale
                , msg = ListingActionLink (Routes.ProjectsRoute <| CreateMigrationRoute questionnaire.uuid)
                , dataCy = "create-migration"
                }

        createMigrationVisible =
            Features.projectCreateMigration appState questionnaire

        continueMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createMigration" appState
                , label = gettext "Continue migration" appState.locale
                , msg = ListingActionLink (Routes.ProjectsRoute <| MigrationRoute questionnaire.uuid)
                , dataCy = "continue-migration"
                }

        continueMigrationVisible =
            Features.projectContinueMigration appState questionnaire

        cancelMigration =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.cancel" appState
                , label = gettext "Cancel migration" appState.locale
                , msg = ListingActionMsg (DeleteQuestionnaireMigration questionnaire.uuid)
                , dataCy = "cancel-migration"
                }

        cancelMigrationVisible =
            Features.projectCancelMigration appState questionnaire

        delete =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = gettext "Delete" appState.locale
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
        |> listInsertIf Listing.dropdownSeparator openProjectVisible
        |> listInsertIf clone cloneVisible
        |> listInsertIf continueMigration continueMigrationVisible
        |> listInsertIf cancelMigration cancelMigrationVisible
        |> listInsertIf createMigration createMigrationVisible
        |> listInsertIf Listing.dropdownSeparator ((cloneVisible || continueMigrationVisible || cancelMigrationVisible || createMigrationVisible) && deleteVisible)
        |> listInsertIf delete deleteVisible


stateBadge : AppState -> Questionnaire -> Html msg
stateBadge appState questionnaire =
    case questionnaire.state of
        Migrating ->
            Badge.info [] [ text (gettext "migrating" appState.locale) ]

        Outdated ->
            linkTo appState
                (Routes.projectsCreateMigration questionnaire.uuid)
                [ class Badge.warningClass, dataCy "badge_project_update-available" ]
                [ text (gettext "update available" appState.locale) ]

        Default ->
            emptyNode


templateBadge : AppState -> Questionnaire -> Html msg
templateBadge appState questionnaire =
    if questionnaire.isTemplate then
        Badge.info [] [ text (gettext "Template" appState.locale) ]

    else
        emptyNode
