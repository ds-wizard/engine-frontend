module Wizard.Pages.Projects.Index.View exposing (view)

import ActionResult
import Bootstrap.Dropdown as Dropdown
import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faCancel, faDelete, faListingFilterMultiNotSelected, faListingFilterMultiSelected, faOpen, faQuestionnaireListClone, faQuestionnaireListCreateMigration, faQuestionnaireListCreateProjectFromTemplate)
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Data.Pagination as Pagination
import Common.Data.PaginationQueryFilters as PaginationQueryFilter
import Common.Data.PaginationQueryFilters.FilterOperator as FilterOperator
import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, input, span, text)
import Html.Attributes exposing (class, classList, placeholder, title, type_, value)
import Html.Attributes.Extensions exposing (dataCy, dataTour)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onClickStopPropagation)
import Html.Extra as Html
import List.Extra as List
import List.Utils as List
import Maybe.Extra as Maybe
import Uuid
import Version
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.PackageSuggestion as PackageSuggestion
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Api.Models.Questionnaire.QuestionnaireState exposing (QuestionnaireState(..))
import Wizard.Api.Models.User as User
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Components.MemberIcon as MemberIcon
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Routes
import Wizard.Pages.Projects.Common.CloneProjectModal.Msgs as CloneProjectModalMsg
import Wizard.Pages.Projects.Common.CloneProjectModal.View as CloneProjectModal
import Wizard.Pages.Projects.Common.DeleteProjectModal.Msgs as DeleteProjectModalMsg
import Wizard.Pages.Projects.Common.DeleteProjectModal.View as DeleteProjectModal
import Wizard.Pages.Projects.Common.QuestionnaireDescriptor as QuestionnaireDescriptor
import Wizard.Pages.Projects.Common.View exposing (visibilityIcon)
import Wizard.Pages.Projects.Index.Models exposing (Model)
import Wizard.Pages.Projects.Index.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Routes exposing (Route(..), indexRouteIsTemplateFilterId, indexRoutePackagesFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Features
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)


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
                , FormResult.view model.deletingMigration
                , Listing.view appState (listingConfig appState model) model.questionnaires
                , Html.map DeleteQuestionnaireModalMsg <| DeleteProjectModal.view appState model.deleteModalModel
                , Html.map CloneQuestionnaireModalMsg <| CloneProjectModal.view appState model.cloneModalModel
                ]
    in
    Page.actionResultView appState content actionResult


createButton : AppState -> Html Msg
createButton appState =
    linkTo Routes.projectsCreate
        [ class "btn btn-primary"
        , dataCy "projects_create-button"
        , dataTour "projects_create-button"
        ]
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
                |> List.insertIf templateFilter (Features.projectTemplatesCreate appState)
                |> List.insertIf tagsFilter (Features.projectTagging appState && tagsFilterVisible)
                |> List.insertIf kmsFilter True
                |> List.insertIf usersFilter True
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
        updateTagsMsg tags =
            let
                value =
                    String.join "," (List.unique tags)

                filters =
                    if String.isEmpty value then
                        PaginationQueryFilter.removeFilter indexRouteProjectTagsFilterId model.questionnaires.filters

                    else
                        PaginationQueryFilter.insertValue indexRouteProjectTagsFilterId value model.questionnaires.filters
            in
            (ListingMsg << ListingMsgs.UpdatePaginationQueryFilters (Just indexRouteProjectTagsFilterId)) filters

        updateOpMsg op =
            (ListingMsg << ListingMsgs.UpdatePaginationQueryFilters (Just indexRouteProjectTagsFilterId))
                (PaginationQueryFilter.insertOp indexRouteProjectTagsFilterId op model.questionnaires.filters)

        removeTagMsg tag =
            updateTagsMsg <| List.filter ((/=) tag) selectedTags

        addTagMsg tag =
            updateTagsMsg <| tag :: selectedTags

        viewTagItem updateMsg icon tag =
            Dropdown.buttonItem
                [ onClick (updateMsg tag)
                , class "dropdown-item-icon"
                , dataCy "project_filter_tags_option"
                ]
                [ icon, text tag ]

        selectedTagItem =
            viewTagItem removeTagMsg faListingFilterMultiSelected

        sortTags =
            List.sortBy String.toUpper

        selectedTags =
            model.questionnaires.filters
                |> PaginationQueryFilter.getValue indexRouteProjectTagsFilterId
                |> Maybe.unwrap [] (sortTags << String.split ",")

        filterTags =
            List.filter (not << flip List.member selectedTags)

        foundTags =
            model.projectTagsFilterTags
                |> ActionResult.unwrap [] (sortTags << filterTags << .items)

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
                        , onClickStopPropagation (ProjectTagsFilterInput model.projectTagsFilterSearchValue)
                        , onInput ProjectTagsFilterInput
                        , value model.projectTagsFilterSearchValue
                        ]
                        []
                    ]
            , Dropdown.divider
            , Dropdown.customItem <|
                div [ class "dropdown-item-operator" ]
                    [ a
                        [ onClick (updateOpMsg FilterOperator.OR)
                        , classList [ ( "active", filterOperator == FilterOperator.OR ) ]
                        , dataCy "filter_projectTags_operator_OR"
                        ]
                        [ text (gettext "OR" appState.locale) ]
                    , a
                        [ onClick (updateOpMsg FilterOperator.AND)
                        , classList [ ( "active", filterOperator == FilterOperator.AND ) ]
                        , dataCy "filter_projectTags_operator_AND"
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
                        viewTagItem addTagMsg faListingFilterMultiNotSelected
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
        filterMsg =
            ListingMsgs.UpdatePaginationQueryFilters (Just indexRoutePackagesFilterId)

        updatePackagesMsg packageIds =
            if List.isEmpty packageIds then
                filterMsg (PaginationQueryFilter.removeFilter indexRoutePackagesFilterId model.questionnaires.filters)

            else
                filterMsg (PaginationQueryFilter.insertValue indexRoutePackagesFilterId (String.join "," (List.unique packageIds)) model.questionnaires.filters)

        removePackageMsg package =
            List.filter ((/=) (PackageSuggestion.packageIdAll package.id)) selectedPackageIds
                |> updatePackagesMsg
                |> ListingMsg

        addPackageMsg package =
            ListingFilterAddSelectedPackage package
                (updatePackagesMsg (PackageSuggestion.packageIdAll package.id :: selectedPackageIds))

        viewPackageItem updateMsg icon package =
            Dropdown.buttonItem
                [ onClick (updateMsg package)
                , class "dropdown-item-icon"
                , dataCy "project_filter_packages_option"
                ]
                [ icon
                , text package.name
                ]

        selectedPackageItem =
            viewPackageItem removePackageMsg faListingFilterMultiSelected

        foundSelectedPackages =
            ActionResult.unwrap [] .items model.packagesFilterSelectedPackages
                |> List.sortBy .name

        selectedPackageIds =
            model.questionnaires.filters
                |> PaginationQueryFilter.getValue indexRoutePackagesFilterId
                |> Maybe.unwrap [] (String.split ",")

        selectedPackages =
            selectedPackageIds
                |> List.filterMap (\a -> List.find (PackageSuggestion.isSamePackage a << .id) foundSelectedPackages)
                |> List.sortBy .name

        filterPackages =
            List.filter (Maybe.isNothing << (\package -> List.find (PackageSuggestion.isSamePackage package.id) selectedPackageIds))

        foundPackages =
            model.packagesFilterPackages
                |> ActionResult.unwrap [] (List.sortBy .name << filterPackages << .items)

        badge =
            filterBadge selectedPackages

        searchInputItem =
            [ Dropdown.customItem <|
                div [ class "dropdown-item-search" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , placeholder (gettext "Search knowledge models..." appState.locale)
                        , onClickStopPropagation (PackagesFilterInput model.packagesFilterSearchValue)
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
                        viewPackageItem addPackageMsg faListingFilterMultiNotSelected
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
        filterMsg =
            ListingMsgs.UpdatePaginationQueryFilters (Just indexRouteUsersFilterId)

        updateUserMsg userUuids =
            if List.isEmpty userUuids then
                filterMsg (PaginationQueryFilter.removeFilter indexRouteUsersFilterId model.questionnaires.filters)

            else
                filterMsg (PaginationQueryFilter.insertValue indexRouteUsersFilterId (String.join "," (List.unique userUuids)) model.questionnaires.filters)

        filtersWithOp op =
            PaginationQueryFilter.insertOp indexRouteUsersFilterId op model.questionnaires.filters

        removeUserMsg user =
            List.filter ((/=) (Uuid.toString user.uuid)) selectedUserUuids
                |> updateUserMsg
                |> ListingMsg

        addUserMsg user =
            ListingFilterAddSelectedUser user
                (updateUserMsg (Uuid.toString user.uuid :: selectedUserUuids))

        viewUserItem updateMsg icon user =
            Dropdown.buttonItem
                [ onClick (updateMsg user)
                , class "dropdown-item-icon"
                , dataCy "project_filter_users_option"
                ]
                [ icon
                , UserIcon.viewSmall user
                , text (User.fullName user)
                ]

        selectedUserItem =
            viewUserItem removeUserMsg faListingFilterMultiSelected

        foundSelectedUsers =
            ActionResult.unwrap [] .items model.userFilterSelectedUsers
                |> List.sortWith User.compare

        selectedUserUuids =
            model.questionnaires.filters
                |> PaginationQueryFilter.getValue indexRouteUsersFilterId
                |> Maybe.unwrap [] (String.split ",")

        selectedUsers =
            selectedUserUuids
                |> List.filterMap (\a -> List.find (\u -> Uuid.toString u.uuid == a) foundSelectedUsers)
                |> List.sortWith User.compare

        filterUsers =
            List.filter (not << flip List.member selectedUserUuids << Uuid.toString << .uuid)

        foundUsers =
            model.userFilterUsers
                |> ActionResult.unwrap [] (List.sortWith User.compare << filterUsers << .items)

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
                        , onClickStopPropagation (UsersFilterInput model.userFilterSearchValue)
                        , onInput UsersFilterInput
                        , value model.userFilterSearchValue
                        ]
                        []
                    ]
            , Dropdown.divider
            , Dropdown.customItem <|
                div [ class "dropdown-item-operator" ]
                    [ a
                        [ classList [ ( "active", filterOperator == FilterOperator.OR ) ]
                        , dataCy "filter_users_operator_OR"
                        , onClickStopPropagation (ListingMsg (filterMsg (filtersWithOp FilterOperator.OR)))
                        ]
                        [ text (gettext "OR" appState.locale) ]
                    , a
                        [ classList [ ( "active", filterOperator == FilterOperator.AND ) ]
                        , dataCy "filter_users_operator_AND"
                        , onClickStopPropagation (ListingMsg (filterMsg (filtersWithOp FilterOperator.AND)))
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
                        viewUserItem addUserMsg faListingFilterMultiNotSelected
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
            Html.nothing

        1 ->
            Html.nothing

        n ->
            Badge.dark [ class "rounded-pill" ] [ text ("+" ++ String.fromInt (n - 1)) ]


listingTitle : AppState -> Questionnaire -> Html Msg
listingTitle appState questionnaire =
    let
        linkRoute =
            if questionnaire.state == Migrating then
                Routes.projectsMigration

            else
                Routes.projectsDetail
    in
    span []
        [ linkTo (linkRoute questionnaire.uuid) [] [ text questionnaire.name ]
        , templateBadge appState questionnaire
        , visibilityIcon appState questionnaire
        , stateBadge appState questionnaire
        ]


listingDescription : AppState -> Questionnaire -> Html Msg
listingDescription appState questionnaire =
    let
        collaborators =
            case questionnaire.permissions of
                [] ->
                    Html.nothing

                perm :: [] ->
                    span [ class "fragment" ]
                        [ MemberIcon.view perm.member
                        , text <| Member.visibleName perm.member
                        ]

                perms ->
                    let
                        users =
                            perms
                                |> List.map .member
                                |> List.sortWith Member.compare
                                |> List.take 5
                                |> List.map MemberIcon.viewIconOnly

                        extraUsers =
                            if List.length perms > 5 then
                                span [] [ text ("+" ++ String.fromInt (List.length perms - 5)) ]

                            else
                                Html.nothing
                    in
                    span [ class "fragment" ] (users ++ [ extraUsers ])

        kmRoute =
            Routes.KnowledgeModelsRoute <|
                Wizard.Pages.KnowledgeModels.Routes.DetailRoute questionnaire.package.id

        kmLink =
            linkTo kmRoute
                [ title <| gettext "Knowledge Model" appState.locale, class "fragment" ]
                [ text questionnaire.package.name
                , Badge.light [ class "ms-1" ] [ text <| Version.toString questionnaire.package.version ]
                ]
    in
    span []
        [ collaborators, kmLink ]


listingActions : AppState -> Questionnaire -> List (ListingDropdownItem Msg)
listingActions appState questionnaire =
    let
        openProject =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faOpen
                , label = gettext "Open project" appState.locale
                , msg = ListingActionLink (Routes.projectsDetail questionnaire.uuid)
                , dataCy = "open"
                }

        openProjectVisible =
            Features.projectOpen appState questionnaire

        createProjectFromTemplate =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faQuestionnaireListCreateProjectFromTemplate
                , label = gettext "Create project from this template" appState.locale
                , msg = ListingActionLink (Routes.projectsCreateFromProjectTemplate questionnaire.uuid)
                , dataCy = "create-project-from-template"
                }

        createProjectFromTemplateVisible =
            Features.projectCreateFromTemplate appState questionnaire

        clone =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faQuestionnaireListClone
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
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faQuestionnaireListCreateMigration
                , label = gettext "Create migration" appState.locale
                , msg = ListingActionLink (Routes.ProjectsRoute <| CreateMigrationRoute questionnaire.uuid)
                , dataCy = "create-migration"
                }

        createMigrationVisible =
            Features.projectCreateMigration appState questionnaire

        continueMigration =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faQuestionnaireListCreateMigration
                , label = gettext "Continue migration" appState.locale
                , msg = ListingActionLink (Routes.ProjectsRoute <| MigrationRoute questionnaire.uuid)
                , dataCy = "continue-migration"
                }

        continueMigrationVisible =
            Features.projectContinueMigration appState questionnaire

        cancelMigration =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faCancel
                , label = gettext "Cancel migration" appState.locale
                , msg = ListingActionMsg (DeleteQuestionnaireMigration questionnaire.uuid)
                , dataCy = "cancel-migration"
                }

        cancelMigrationVisible =
            Features.projectCancelMigration appState questionnaire

        delete =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faDelete
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

        groups =
            [ [ ( openProject, openProjectVisible ) ]
            , [ ( createProjectFromTemplate, createProjectFromTemplateVisible ) ]
            , [ ( clone, cloneVisible )
              , ( continueMigration, continueMigrationVisible )
              , ( cancelMigration, cancelMigrationVisible )
              , ( createMigration, createMigrationVisible )
              ]
            , [ ( delete, deleteVisible ) ]
            ]
    in
    ListingDropdown.itemsFromGroups groups


stateBadge : AppState -> Questionnaire -> Html msg
stateBadge appState questionnaire =
    case questionnaire.state of
        Migrating ->
            linkTo (Routes.projectsMigration questionnaire.uuid)
                [ class Badge.infoClass, dataCy "badge_project_migrating" ]
                [ faQuestionnaireListCreateMigration
                , text (gettext "migrating" appState.locale)
                ]

        Outdated ->
            linkTo (Routes.projectsCreateMigration questionnaire.uuid)
                [ class Badge.warningClass, dataCy "badge_project_update-available" ]
                [ text (gettext "update available" appState.locale) ]

        Default ->
            Html.nothing


templateBadge : AppState -> Questionnaire -> Html msg
templateBadge appState questionnaire =
    if questionnaire.isTemplate then
        Badge.info [] [ text (gettext "Template" appState.locale) ]

    else
        Html.nothing
