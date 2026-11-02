module Common.Pages.Projects.Index exposing
    ( Model
    , Msg
    , RouteParams
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| The projects listing.

The richest of the listings: an "is template" filter, and search-backed filters
for project tags, knowledge models, users and user groups, each asking the same
suggestion endpoints the wizard asks. The users, user groups and project tags
filters combine their values with AND or OR; a project has one knowledge model,
so that filter, as in the wizard, has no operator.

The project tags filter is offered only when tagging is on and there is a tag to
choose, or one is already chosen (`Listing.searchFilterAvailable`).

-}

import Browser.Dom as Dom
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.KnowledgeModelPackageSuggestion as KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Common.Api.Models.Member as Member exposing (Member)
import Common.Api.Models.Project exposing (Project)
import Common.Api.Models.Project.DocumentTemplateProjectState as DocumentTemplateProjectState
import Common.Api.Models.Project.KnowledgeModelProjectState as KnowledgeModelProjectState
import Common.Api.Models.User as User
import Common.Api.Models.UserGroupSuggestion exposing (UserGroupSuggestion)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Api.Projects as ProjectsApi
import Common.Api.UserGroups as UserGroupsApi
import Common.Api.Users as UsersApi
import Common.Components.Listing as Listing
import Common.Components.Timestamp as Timestamp
import Common.Components.Tour as Tour
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.Session as Session
import Common.Data.TourId as TourId
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Projects.Components.CloneProjectModal as CloneProjectModal
import Common.Pages.Projects.Components.DeleteProjectModal as DeleteProjectModal
import Common.Pages.Projects.Components.ProjectActions as ProjectActions
import Common.Pages.Projects.Components.ProjectDescriptor as ProjectDescriptor
import Common.Pages.Projects.Components.ProjectSharing as ProjectSharing
import Common.Pages.Projects.Routes exposing (indexRouteIsTemplateFilterId, indexRouteKnowledgeModelPackagesFilterId, indexRouteProjectTagsFilterId, indexRouteUserGroupsFilterId, indexRouteUsersFilterId)
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html, span, text)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Task.Extra as Task
import Ui.AvatarGroup as AvatarGroup
import Ui.Badge as Badge
import Ui.Button as Button exposing (Button)
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { projects : Listing.Model Project
    , deleteModal : DeleteProjectModal.Model
    , cloneModal : CloneProjectModal.Model
    }


initialModel : Model
initialModel =
    { projects = Listing.initialModel PaginationQueryString.empty
    , deleteModal = DeleteProjectModal.initialModel
    , cloneModal = CloneProjectModal.initialModel
    }


type alias RouteParams =
    { paginationQueryString : PaginationQueryString
    , isTemplate : Maybe String
    , users : Maybe String
    , usersOp : Maybe FilterOperator
    , userGroups : Maybe String
    , userGroupsOp : Maybe FilterOperator
    , projectTags : Maybe String
    , projectTagsOp : Maybe FilterOperator
    , packages : Maybe String
    , packagesOp : Maybe FilterOperator
    }


init : AppState -> RouteParams -> ( Model, Cmd Msg )
init appState params =
    let
        filters =
            PaginationQueryFilters.create
                [ ( indexRouteIsTemplateFilterId, params.isTemplate )
                , ( indexRouteUsersFilterId, params.users )
                , ( indexRouteUserGroupsFilterId, params.userGroups )
                , ( indexRouteProjectTagsFilterId, params.projectTags )
                , ( indexRouteKnowledgeModelPackagesFilterId, params.packages )
                ]
                [ ( indexRouteUsersFilterId, params.usersOp )
                , ( indexRouteUserGroupsFilterId, params.userGroupsOp )
                , ( indexRouteProjectTagsFilterId, params.projectTagsOp )
                , ( indexRouteKnowledgeModelPackagesFilterId, params.packagesOp )
                ]

        ( projects, listingCmd ) =
            Listing.init params.paginationQueryString filters
    in
    ( { initialModel | projects = projects }
    , Cmd.batch
        [ Cmd.map ListingMsg listingCmd
        , tour appState
        ]
    )


tour : AppState -> Cmd msg
tour appState =
    Tour.tour appState
        { tourId = TourId.projectsIndex
        , completedTours = appState.config.tours
        , enabled = appState.config.features.toursEnabled && Session.exists appState.session
        , steps =
            { element = Tour.centered
            , title = gettext "Projects" appState.locale
            , description = gettext "All your projects will be here, including those shared with you." appState.locale
            }
                :: (if Feature.projectsCreate appState then
                        [ { element = Tour.byTestId createButtonTestId
                          , title = gettext "Create Project" appState.locale
                          , description = gettext "Click here to start a new project." appState.locale
                          }
                        ]

                    else
                        []
                   )
        }



-- UPDATE


type Msg
    = ListingMsg (Listing.Msg Project)
    | DeleteModalMsg DeleteProjectModal.Msg
    | CloneModalMsg CloneProjectModal.Msg
    | OpenDeleteModal Project
    | OpenCloneModal Project
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ListingMsg listingMsg ->
            let
                ( projects, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.projects
            in
            ( { model | projects = projects }, cmd )

        DeleteModalMsg modalMsg ->
            let
                ( deleteModal, cmd ) =
                    DeleteProjectModal.update
                        { wrapMsg = cfg.wrapMsg << DeleteModalMsg
                        , logoutMsg = cfg.logoutMsg
                        , deleteCompleteCmd = Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete))
                        , closeCmd = focusRowActions cfg
                        }
                        appState
                        modalMsg
                        model.deleteModal
            in
            ( { model | deleteModal = deleteModal }, cmd )

        CloneModalMsg modalMsg ->
            let
                ( cloneModal, cmd ) =
                    CloneProjectModal.update
                        { wrapMsg = cfg.wrapMsg << CloneModalMsg
                        , logoutMsg = cfg.logoutMsg
                        , cloneCompleteCmd = Routes.navigate appState << Routes.projectsDetail << .uuid
                        , closeCmd = focusRowActions cfg
                        }
                        appState
                        modalMsg
                        model.cloneModal
            in
            ( { model | cloneModal = cloneModal }, cmd )

        OpenDeleteModal project ->
            update cfg appState (DeleteModalMsg (DeleteProjectModal.open (ProjectDescriptor.fromProject project))) (closeMenus cfg appState model)

        OpenCloneModal project ->
            update cfg appState (CloneModalMsg (CloneProjectModal.open (ProjectDescriptor.fromProject project))) (closeMenus cfg appState model)

        NoOp ->
            ( model, Cmd.none )


{-| A dialog a row's action opened was dismissed and the row is still there:
focus goes back to the row's menu.
-}
focusRowActions : UpdateConfig Msg msg -> Uuid -> Cmd msg
focusRowActions cfg uuid =
    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Listing.itemActionsTriggerId (Uuid.toString uuid)))


{-| The row menu a dialog was opened from closes under it, leaving focus to the
dialog.
-}
closeMenus : UpdateConfig Msg msg -> AppState -> Model -> Model
closeMenus cfg appState model =
    { model | projects = Tuple.first (Listing.update appState (listingUpdateConfig cfg appState) Listing.closeMenus model.projects) }


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig Project msg
listingUpdateConfig cfg appState =
    { getRequest = ProjectsApi.getList appState
    , getError = gettext "Unable to get projects." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.projectsIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters =
        [ ( indexRouteProjectTagsFilterId, projectTagsSource appState )
        , ( indexRouteKnowledgeModelPackagesFilterId, packagesSource appState )
        , ( indexRouteUsersFilterId, usersSource appState )
        ]
            ++ [ ( indexRouteUserGroupsFilterId, userGroupsSource appState ) ]
    }


suggestionsQueryString : String -> PaginationQueryString
suggestionsQueryString query =
    PaginationQueryString.fromQ query
        |> PaginationQueryString.withSize (Just 10)


projectTagsSource : AppState -> Listing.SearchFilterSource msg
projectTagsSource appState =
    { search =
        \{ query, selected } toMsg ->
            ProjectsApi.getProjectTagsSuggestions appState
                (suggestionsQueryString query)
                selected
                (toMsg << Result.map (List.map (\tag -> { id = tag, label = tag }) << List.sortBy String.toUpper << .items))
    , resolve = Nothing
    , getError = gettext "Unable to get project tags." appState.locale
    }


packagesSource : AppState -> Listing.SearchFilterSource msg
packagesSource appState =
    let
        toSuggestions =
            List.map packageSuggestion << List.sortBy .name << .items
    in
    { search =
        \{ query, selected } toMsg ->
            KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestionsWithOptions appState
                (suggestionsQueryString query)
                []
                selected
                (toMsg << Result.map toSuggestions)
    , resolve =
        Just
            (\ids toMsg ->
                KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestionsWithOptions appState
                    PaginationQueryString.empty
                    ids
                    []
                    (toMsg << Result.map toSuggestions)
            )
    , getError = gettext "Unable to get knowledge models." appState.locale
    }


packageSuggestion : KnowledgeModelPackageSuggestion -> Listing.Suggestion
packageSuggestion kmPackage =
    { id = KnowledgeModelPackageSuggestion.knowledgeModelPackageIdAll kmPackage.id
    , label = kmPackage.name
    }


usersSource : AppState -> Listing.SearchFilterSource msg
usersSource appState =
    let
        toSuggestions =
            List.map userSuggestion << List.sortWith User.compare << .items
    in
    { search =
        \{ query, selected } toMsg ->
            UsersApi.getUsersSuggestionsWithOptions appState
                (suggestionsQueryString query)
                []
                selected
                (toMsg << Result.map toSuggestions)
    , resolve =
        Just
            (\ids toMsg ->
                UsersApi.getUsersSuggestionsWithOptions appState
                    PaginationQueryString.empty
                    ids
                    []
                    (toMsg << Result.map toSuggestions)
            )
    , getError = gettext "Unable to get users." appState.locale
    }


userSuggestion : UserSuggestion -> Listing.Suggestion
userSuggestion user =
    { id = Uuid.toString user.uuid
    , label = User.fullName user
    }


userGroupsSource : AppState -> Listing.SearchFilterSource msg
userGroupsSource appState =
    let
        toSuggestions =
            List.map userGroupSuggestion << List.sortBy .name << .items
    in
    { search =
        \{ query, selected } toMsg ->
            UserGroupsApi.getUserGroupsSuggestionsWithOptions appState
                (suggestionsQueryString query)
                []
                selected
                (toMsg << Result.map toSuggestions)
    , resolve =
        Just
            (\ids toMsg ->
                UserGroupsApi.getUserGroupsSuggestionsWithOptions appState
                    PaginationQueryString.empty
                    ids
                    []
                    (toMsg << Result.map toSuggestions)
            )
    , getError = gettext "Unable to get user groups." appState.locale
    }


userGroupSuggestion : UserGroupSuggestion -> Listing.Suggestion
userGroupSuggestion userGroup =
    { id = Uuid.toString userGroup.uuid
    , label = userGroup.name
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.projects)



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Listing.viewWith appState
        { notices = []
        , overlays =
            [ Html.map DeleteModalMsg (DeleteProjectModal.view appState model.deleteModal)
            , Html.map CloneModalMsg (CloneProjectModal.view appState model.cloneModal)
            ]
        }
        (listingConfig appState model)
        model.projects


createButtonTestId : String
createButtonTestId =
    "projects_create-button"


createButton : AppState -> Button msg
createButton appState =
    Button.linkPrimary (gettext "Create" appState.locale) (Routes.toUrl appState Routes.projectsCreate)
        |> Button.withIcon (Icon.solid "fa-plus")
        |> Button.withTestId createButtonTestId


{-| What the client offers beside "Create" (`AppState.projectListLinks`), before
it: "Create" is the primary action, and the primary action comes last.
-}
toolbarActions : AppState -> List (Button msg)
toolbarActions appState =
    List.map
        (\link ->
            Button.link link.label link.url
                |> Button.withIcon (Icon.solid link.icon)
                |> Button.withTestId link.testId
        )
        appState.projectListLinks
        ++ (if Feature.projectsCreate appState then
                [ createButton appState ]

            else
                []
           )


listingConfig : AppState -> Model -> Listing.ViewConfig Project Msg
listingConfig appState model =
    let
        tagsFilterVisible =
            Feature.projectTagging appState
                && Listing.searchFilterAvailable indexRouteProjectTagsFilterId model.projects

        filters =
            List.concat
                [ if Feature.projectTemplatesCreate appState then
                    [ Listing.SingleFilter indexRouteIsTemplateFilterId
                        { name = gettext "Project Template" appState.locale
                        , options =
                            [ ( "true", gettext "Templates only" appState.locale )
                            , ( "false", gettext "Projects only" appState.locale )
                            ]
                        }
                    ]

                  else
                    []
                , if tagsFilterVisible then
                    [ Listing.SearchFilter indexRouteProjectTagsFilterId
                        { name = gettext "Project Tags" appState.locale
                        , searchPlaceholder = gettext "Search project tags..." appState.locale
                        , noResults = gettext "No project tags found" appState.locale
                        , operator = True
                        }
                    ]

                  else
                    []
                , [ Listing.SearchFilter indexRouteKnowledgeModelPackagesFilterId
                        { name = gettext "Knowledge Models" appState.locale
                        , searchPlaceholder = gettext "Search knowledge models..." appState.locale
                        , noResults = gettext "No knowledge models found" appState.locale
                        , operator = False
                        }
                  , Listing.SearchFilter indexRouteUsersFilterId
                        { name = gettext "Users" appState.locale
                        , searchPlaceholder = gettext "Search users..." appState.locale
                        , noResults = gettext "No users found" appState.locale
                        , operator = True
                        }
                  , Listing.SearchFilter indexRouteUserGroupsFilterId
                        { name = gettext "User Groups" appState.locale
                        , searchPlaceholder = gettext "Search user groups..." appState.locale
                        , noResults = gettext "No user groups found" appState.locale
                        , operator = True
                        }
                  ]
                ]
    in
    { title = gettext "Projects" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions = toolbarActions appState
    , searchPlaceholder = gettext "Search projects..." appState.locale
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        , ( "updatedAt", gettext "Updated" appState.locale )
        ]
    , filters = filters
    , item = listingItem appState
    , emptyTitle = gettext "No projects" appState.locale
    , emptyDescription =
        if Feature.projectsCreate appState then
            gettext "Click \"Create\" button to add a new project." appState.locale

        else
            gettext "There are no projects." appState.locale
    , wrapMsg = ListingMsg
    }


listingItem : AppState -> Project -> Listing.Item Msg
listingItem appState project =
    let
        row =
            Listing.item
                { id = Uuid.toString project.uuid
                , title =
                    -- Built here rather than through the row's `badges`: the
                    -- sharing icon belongs right after the name, and the
                    -- outdated badges are links, which a plain badge is not.
                    Layout.inline
                        (Link.toHtml
                            (Link.link
                                { label = project.name
                                , url = Routes.toUrl appState (Routes.projectsDetail project.uuid)
                                }
                            )
                            :: ProjectSharing.view appState project
                            :: badges appState project
                        )
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ project.name ]
                }
    in
    { row
        | icon = Just (ItemIcon.toHtml (UserAvatar.item project.name Nothing))
        , fragments =
            Maybe.values
                [ members appState project
                , Just (knowledgeModelFragment appState project)
                ]
        , timestamp = Just (Timestamp.updated appState project.updatedAt)
        , actions =
            ProjectActions.actions appState
                { clone = OpenCloneModal
                , delete = OpenDeleteModal
                }
                project
    }


{-| The outdated badges go to the project's settings, where the knowledge model
or the document template is updated, as they did in the wizard.
-}
badges : AppState -> Project -> List (Html msg)
badges appState project =
    let
        settingsUrl =
            Routes.toUrl appState (Routes.projectsDetailSettings project.uuid)
    in
    Maybe.values
        [ if project.isTemplate then
            Just (Badge.toHtml (Badge.info (gettext "Template" appState.locale)))

          else
            Nothing
        , if project.knowledgeModelState == KnowledgeModelProjectState.Outdated then
            Badge.warning (gettext "Outdated KM" appState.locale)
                |> Badge.withDescription (gettext "Knowledge model update available" appState.locale)
                |> Badge.toLink settingsUrl
                |> Just

          else
            Nothing
        , if project.documentTemplateState == Just DocumentTemplateProjectState.Outdated then
            Badge.warning (gettext "Outdated DT" appState.locale)
                |> Badge.withDescription (gettext "Document template update available" appState.locale)
                |> Badge.toLink settingsUrl
                |> Just

          else
            Nothing
        ]


{-| One member is shown with their name; several as a row of up to five faces,
each named, and a count of the rest.
-}
members : AppState -> Project -> Maybe (Html msg)
members appState project =
    case List.map .member project.permissions of
        [] ->
            Nothing

        member :: [] ->
            -- One line: the icon is a block, so beside plain text it broke
            -- onto a line of its own.
            Just
                (Layout.inline
                    [ ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Small (memberIcon member))
                    , text (Member.visibleName member)
                    ]
                )

        projectMembers ->
            Just
                (AvatarGroup.toHtml
                    { label = gettext "Members" appState.locale
                    , visible = 5
                    , people = List.map memberPerson (List.sortWith Member.compare projectMembers)
                    }
                )


memberIcon : Member -> ItemIcon.ItemIcon
memberIcon member =
    case Member.toUserSuggestion member of
        Just user ->
            UserAvatar.user
                { firstName = user.firstName
                , lastName = user.lastName
                , imageUrl = user.imageUrl
                , gravatarHash = Just user.gravatarHash
                }

        Nothing ->
            UserAvatar.group (Member.visibleName member)


memberPerson : Member -> AvatarGroup.Person
memberPerson member =
    case Member.toUserSuggestion member of
        Just user ->
            UserAvatar.person
                { firstName = user.firstName
                , lastName = user.lastName
                , imageUrl = user.imageUrl
                , gravatarHash = Just user.gravatarHash
                }

        Nothing ->
            AvatarGroup.person (Member.visibleName member)


knowledgeModelFragment : AppState -> Project -> Html msg
knowledgeModelFragment appState project =
    span []
        [ Link.toHtml
            (Link.link
                { label = project.knowledgeModelPackage.name
                , url = Routes.toUrl appState (Routes.knowledgeModelsDetail project.knowledgeModelPackage.uuid)
                }
            )
        , text " "
        , Badge.toHtml (Badge.neutral (Version.toString project.knowledgeModelPackage.version))
        ]
