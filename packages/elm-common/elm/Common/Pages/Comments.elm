module Common.Pages.Comments exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| Comments: the feature router, and its one page — the comment
threads assigned to the current user.

This feature has no nested route type, so every function takes the arguments of
`Common.Routes.CommentsRoute` — the pagination query string and the `resolved`
filter — where other routers take their route.

Each thread links to its place in the project's questionnaire. That page is a
placeholder for now; the link is still the real route, so it starts working when
the project detail does.

-}

import Common.Api.Models.ProjectCommentThreadAssigned exposing (ProjectCommentThreadAssigned)
import Common.Api.Models.User as User
import Common.Api.ProjectCommentThreads as ProjectCommentThreadsApi
import Common.Components.Listing as Listing
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Routes as Routes exposing (commentsRouteResolvedFilterId)
import Gettext exposing (gettext)
import Html exposing (Html)
import Random
import Ui.Badge as Badge
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Uuid


type alias Model =
    { commentThreads : Listing.Model ProjectCommentThreadAssigned
    }


initialModel : AppState -> Model
initialModel =
    \_ ->
        { commentThreads = Listing.initialModel PaginationQueryString.empty
        }


init : AppState -> Model -> PaginationQueryString -> Maybe String -> ( Model, Cmd Msg )
init =
    \_ _ paginationQueryString mbResolved ->
        let
            ( commentThreads, cmd ) =
                Listing.init paginationQueryString
                    (PaginationQueryFilters.create [ ( commentsRouteResolvedFilterId, mbResolved ) ] [])
        in
        ( { commentThreads = commentThreads }, Cmd.map ListingMsg cmd )


type Msg
    = ListingMsg (Listing.Msg ProjectCommentThreadAssigned)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ListingMsg listingMsg ->
            let
                ( commentThreads, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.commentThreads
            in
            ( appState.seed, { model | commentThreads = commentThreads }, cmd )


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig ProjectCommentThreadAssigned msg
listingUpdateConfig cfg appState =
    { getRequest = ProjectCommentThreadsApi.getCommentThreads appState
    , getError = gettext "Unable to get comments." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.commentsIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }


subscriptions : AppState -> PaginationQueryString -> Maybe String -> Model -> Sub Msg
subscriptions =
    \_ _ _ model ->
        Sub.map ListingMsg (Listing.subscriptions model.commentThreads)


view : AppState -> PaginationQueryString -> Maybe String -> Model -> Html Msg
view =
    \appState _ _ model ->
        Listing.view appState (listingConfig appState) model.commentThreads


listingConfig : AppState -> Listing.ViewConfig ProjectCommentThreadAssigned Msg
listingConfig appState =
    { title = gettext "Assigned Comments" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions = []
    , searchPlaceholder = gettext "Search comments..." appState.locale
    , sortOptions =
        [ ( "projectUuid", gettext "Project" appState.locale )
        , ( "updatedAt", gettext "Updated" appState.locale )
        ]
    , filters =
        [ Listing.SingleFilter commentsRouteResolvedFilterId
            { name = gettext "Resolved" appState.locale
            , options =
                [ ( "true", gettext "Resolved only" appState.locale )
                , ( "false", gettext "Unresolved only" appState.locale )
                ]
            }
        ]
    , item = listingItem appState
    , emptyTitle = gettext "No assigned comments" appState.locale
    , emptyDescription = gettext "No comments have been assigned to you." appState.locale
    , wrapMsg = ListingMsg
    }


listingItem : AppState -> ProjectCommentThreadAssigned -> Listing.Item Msg
listingItem appState commentThread =
    let
        url =
            Routes.toUrl appState (Routes.projectsDetailQuestionnaire commentThread.projectUuid (Just commentThread.path) (Just commentThread.commentThreadUuid))

        item =
            Listing.item
                { id = Uuid.toString commentThread.commentThreadUuid
                , title = Link.toHtml (Link.link { label = commentThread.text, url = url })

                -- A comment thread has no actions, so the menu this names is
                -- never rendered.
                , actionsLabel = commentThread.projectName
                }

        badges =
            if commentThread.resolved then
                [ Badge.success (gettext "Resolved" appState.locale) ]

            else
                []

        userFragment =
            case commentThread.createdBy of
                Just createdBy ->
                    [ Layout.inline
                        [ UserAvatar.user
                            { firstName = createdBy.firstName
                            , lastName = createdBy.lastName
                            , imageUrl = createdBy.imageUrl
                            , gravatarHash = Just createdBy.gravatarHash
                            }
                            |> ItemIcon.withSize ItemIcon.Small
                            |> ItemIcon.toHtml
                        , Html.text (User.fullName createdBy)
                        ]
                    ]

                Nothing ->
                    []
    in
    { item
        | icon = Just (ItemIcon.toHtml (UserAvatar.item commentThread.projectName Nothing))
        , badges = badges
        , fragments = Html.text commentThread.projectName :: userFragment
        , timestamp = Just (Timestamp.updated appState commentThread.updatedAt)
    }


isGuarded : AppState -> PaginationQueryString -> Maybe String -> Model -> Maybe String
isGuarded =
    \_ _ _ _ ->
        Nothing
