module Common.Pages.Users.Edit exposing
    ( ActiveSection(..)
    , Model
    , Msg
    , entityName
    , init
    , initialModel
    , navigationGroups
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.User as User exposing (User)
import Common.Api.Users as UsersApi
import Common.Components.Page as Page
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Common.Pages.Users.Edit.ActiveSessions as ActiveSessions
import Common.Pages.Users.Edit.ApiKeys as ApiKeys
import Common.Pages.Users.Edit.ConnectedAccounts as ConnectedAccounts
import Common.Pages.Users.Edit.Language as Language
import Common.Pages.Users.Edit.Password as Password
import Common.Pages.Users.Edit.PluginSettings as PluginSettings
import Common.Pages.Users.Edit.Profile as Profile
import Common.Pages.Users.Edit.Routes as UserEditRoute exposing (UserEditRoute)
import Common.Pages.Users.Edit.SubmissionSettings as SubmissionSettings
import Common.Pages.Users.Edit.Tours as Tours
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Uuid



-- MODEL


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , user : ActionResult User
    , profileModel : Profile.Model
    , passwordModel : Password.Model
    , connectedAccountsModel : ConnectedAccounts.Model
    , languageModel : Language.Model
    , toursModel : Tours.Model
    , apiKeysModel : ApiKeys.Model
    , activeSessionsModel : ActiveSessions.Model
    , submissionSettingsModel : SubmissionSettings.Model
    , pluginSettingsModel : PluginSettings.Model
    }


initialModel : AppState -> UuidOrCurrent -> Model
initialModel appState uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , user = ActionResult.Loading
    , profileModel = Profile.initialModel uuidOrCurrent
    , passwordModel = Password.initialModel appState uuidOrCurrent
    , connectedAccountsModel = ConnectedAccounts.initialModel
    , languageModel = Language.initialModel
    , toursModel = Tours.initialModel
    , apiKeysModel = ApiKeys.initialModel uuidOrCurrent
    , activeSessionsModel = ActiveSessions.initialModel
    , submissionSettingsModel = SubmissionSettings.initialModel
    , pluginSettingsModel = PluginSettings.initialModel uuidOrCurrent Uuid.nil
    }


{-| Resets the model of the tab being opened, keeps the others, and loads the
user and the tab's data.
-}
init : AppState -> UuidOrCurrent -> UserEditRoute -> Model -> ( Model, Cmd Msg )
init appState uuidOrCurrent subroute model =
    let
        updatedModel =
            case subroute of
                UserEditRoute.Profile ->
                    { model | profileModel = Profile.initialModel uuidOrCurrent }

                UserEditRoute.Password ->
                    { model | passwordModel = Password.initialModel appState uuidOrCurrent }

                UserEditRoute.ConnectedAccounts ->
                    { model | connectedAccountsModel = ConnectedAccounts.initialModel }

                UserEditRoute.Language ->
                    { model | languageModel = Language.initialModel }

                UserEditRoute.Tours ->
                    { model | toursModel = Tours.initialModel }

                UserEditRoute.ApiKeys ->
                    { model | apiKeysModel = ApiKeys.initialModel uuidOrCurrent }

                UserEditRoute.ActiveSessions ->
                    { model | activeSessionsModel = ActiveSessions.initialModel }

                UserEditRoute.SubmissionSettings ->
                    { model | submissionSettingsModel = SubmissionSettings.initialModel }

                UserEditRoute.PluginSettings pluginUuid ->
                    { model | pluginSettingsModel = PluginSettings.initialModel uuidOrCurrent pluginUuid }
    in
    ( { updatedModel | uuidOrCurrent = uuidOrCurrent, user = ActionResult.Loading }
    , Cmd.batch
        [ UsersApi.getUser appState uuidOrCurrent GetUserCompleted
        , fetchSubrouteData appState uuidOrCurrent subroute
        ]
    )


fetchSubrouteData : AppState -> UuidOrCurrent -> UserEditRoute -> Cmd Msg
fetchSubrouteData appState uuidOrCurrent subroute =
    case subroute of
        UserEditRoute.Profile ->
            Cmd.map ProfileMsg (Profile.fetchData appState uuidOrCurrent)

        UserEditRoute.Password ->
            Cmd.none

        UserEditRoute.ConnectedAccounts ->
            Cmd.map ConnectedAccountsMsg (ConnectedAccounts.fetchData appState)

        UserEditRoute.Language ->
            Cmd.map LanguageMsg (Language.fetchData appState)

        UserEditRoute.Tours ->
            Tours.fetchData appState

        UserEditRoute.ApiKeys ->
            Cmd.map ApiKeysMsg (ApiKeys.fetchData appState)

        UserEditRoute.ActiveSessions ->
            Cmd.map ActiveSessionsMsg (ActiveSessions.fetchData appState)

        UserEditRoute.SubmissionSettings ->
            Cmd.map SubmissionSettingsMsg (SubmissionSettings.fetchData appState)

        UserEditRoute.PluginSettings pluginUuid ->
            Cmd.map PluginSettingsMsg (PluginSettings.fetchData appState pluginUuid)



-- UPDATE


type Msg
    = GetUserCompleted (Result ApiError User)
    | ProfileMsg Profile.Msg
    | PasswordMsg Password.Msg
    | ConnectedAccountsMsg ConnectedAccounts.Msg
    | LanguageMsg Language.Msg
    | ToursMsg Tours.Msg
    | ApiKeysMsg ApiKeys.Msg
    | ActiveSessionsMsg ActiveSessions.Msg
    | SubmissionSettingsMsg SubmissionSettings.Msg
    | PluginSettingsMsg PluginSettings.Msg


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetUserCompleted result ->
            case result of
                Ok user ->
                    ( { model
                        | user = ActionResult.Success user
                        , profileModel = Profile.setUser user model.profileModel
                      }
                    , Cmd.none
                    )

                Err error ->
                    if ApiError.isNotFound error then
                        ( model
                        , UpdateConfig.notFound cfg (NotFound.user appState)
                        )

                    else
                        ( { model | user = ApiError.toActionResult appState (gettext "Unable to get the user." appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        ProfileMsg subMsg ->
            let
                ( subModel, cmd ) =
                    Profile.update (UpdateConfig.map ProfileMsg cfg) appState subMsg model.profileModel
            in
            ( { model | profileModel = subModel }, cmd )

        PasswordMsg subMsg ->
            let
                ( subModel, cmd ) =
                    Password.update (UpdateConfig.map PasswordMsg cfg) appState subMsg model.passwordModel
            in
            ( { model | passwordModel = subModel }, cmd )

        ConnectedAccountsMsg subMsg ->
            let
                ( subModel, cmd ) =
                    ConnectedAccounts.update (UpdateConfig.map ConnectedAccountsMsg cfg) appState subMsg model.connectedAccountsModel
            in
            ( { model | connectedAccountsModel = subModel }, cmd )

        LanguageMsg subMsg ->
            let
                ( subModel, cmd ) =
                    Language.update (UpdateConfig.map LanguageMsg cfg) appState subMsg model.languageModel
            in
            ( { model | languageModel = subModel }, cmd )

        ToursMsg subMsg ->
            let
                ( subModel, cmd ) =
                    Tours.update (UpdateConfig.map ToursMsg cfg) appState subMsg model.toursModel
            in
            ( { model | toursModel = subModel }, cmd )

        ApiKeysMsg subMsg ->
            let
                ( subModel, cmd ) =
                    ApiKeys.update (UpdateConfig.map ApiKeysMsg cfg) appState subMsg model.apiKeysModel
            in
            ( { model | apiKeysModel = subModel }, cmd )

        ActiveSessionsMsg subMsg ->
            let
                ( subModel, cmd ) =
                    ActiveSessions.update (UpdateConfig.map ActiveSessionsMsg cfg) appState subMsg model.activeSessionsModel
            in
            ( { model | activeSessionsModel = subModel }, cmd )

        SubmissionSettingsMsg subMsg ->
            let
                ( subModel, cmd ) =
                    SubmissionSettings.update (UpdateConfig.map SubmissionSettingsMsg cfg) appState subMsg model.submissionSettingsModel
            in
            ( { model | submissionSettingsModel = subModel }, cmd )

        PluginSettingsMsg subMsg ->
            let
                ( subModel, cmd ) =
                    PluginSettings.update (UpdateConfig.map PluginSettingsMsg cfg) appState subMsg model.pluginSettingsModel
            in
            ( { model | pluginSettingsModel = subModel }, cmd )



-- SUBSCRIPTIONS


subscriptions : AppState -> UserEditRoute -> Model -> Sub Msg
subscriptions appState subroute model =
    case subroute of
        UserEditRoute.Profile ->
            Sub.map ProfileMsg (Profile.subscriptions appState model.profileModel)

        UserEditRoute.Password ->
            Sub.map PasswordMsg (Password.subscriptions appState model.passwordModel)

        UserEditRoute.Language ->
            Sub.map LanguageMsg (Language.subscriptions appState model.languageModel)

        UserEditRoute.ApiKeys ->
            Sub.map ApiKeysMsg (ApiKeys.subscriptions appState model.apiKeysModel)

        UserEditRoute.SubmissionSettings ->
            Sub.map SubmissionSettingsMsg (SubmissionSettings.subscriptions appState model.submissionSettingsModel)

        UserEditRoute.PluginSettings _ ->
            Sub.map PluginSettingsMsg (PluginSettings.subscriptions appState model.pluginSettingsModel)

        _ ->
            Sub.none



-- VIEW


view : AppState -> UserEditRoute -> Model -> Html Msg
view appState subroute model =
    Page.actionResult appState (\_ -> userView appState subroute model) model.user


userView : AppState -> UserEditRoute -> Model -> Html Msg
userView appState subroute model =
    let
        mapBody toMsg =
            List.map (Html.map toMsg)

        ( body, footer ) =
            case subroute of
                UserEditRoute.Profile ->
                    ( mapBody ProfileMsg (Profile.view appState model.profileModel)
                    , Maybe.map (Html.map ProfileMsg) (Profile.footer appState model.profileModel)
                    )

                UserEditRoute.Password ->
                    ( mapBody PasswordMsg (Password.view appState model.passwordModel)
                    , Maybe.map (Html.map PasswordMsg) (Password.footer appState model.passwordModel)
                    )

                UserEditRoute.ConnectedAccounts ->
                    ( mapBody ConnectedAccountsMsg (ConnectedAccounts.view appState model.connectedAccountsModel), Nothing )

                UserEditRoute.Language ->
                    ( mapBody LanguageMsg (Language.view appState model.languageModel)
                    , Maybe.map (Html.map LanguageMsg) (Language.footer appState model.languageModel)
                    )

                UserEditRoute.Tours ->
                    ( mapBody ToursMsg (Tours.view appState model.toursModel), Nothing )

                UserEditRoute.ApiKeys ->
                    ( mapBody ApiKeysMsg (ApiKeys.view appState model.apiKeysModel), Nothing )

                UserEditRoute.ActiveSessions ->
                    ( mapBody ActiveSessionsMsg (ActiveSessions.view appState model.activeSessionsModel), Nothing )

                UserEditRoute.SubmissionSettings ->
                    ( mapBody SubmissionSettingsMsg (SubmissionSettings.view appState model.submissionSettingsModel)
                    , Maybe.map (Html.map SubmissionSettingsMsg) (SubmissionSettings.footer appState model.submissionSettingsModel)
                    )

                UserEditRoute.PluginSettings _ ->
                    ( mapBody PluginSettingsMsg (PluginSettings.view appState model.pluginSettingsModel)
                    , Maybe.map (Html.map PluginSettingsMsg) (PluginSettings.footer appState model.pluginSettingsModel)
                    )
    in
    SettingsPage.layoutWithTestIds
        { title = gettext "User Settings" appState.locale
        , groups = navigationGroups appState model.uuidOrCurrent (SharedSection subroute)
        , body = body
        , footer = footer
        }


{-| Which section of a user's settings is shown: one of these pages, or one
a client adds (`AppState.userSettingsLinks`), by its id.
-}
type ActiveSection
    = SharedSection UserEditRoute
    | ClientSection String


{-| The navigation of a user's settings. A client draws its own sections with
it, in `Common.Components.SettingsPage`, so they sit in the same menu.
-}
navigationGroups : AppState -> UuidOrCurrent -> ActiveSection -> List ( Maybe String, List { label : String, url : String, active : Bool, testId : Maybe String } )
navigationGroups appState uuidOrCurrent activeSection =
    let
        navItem ( ( visible, testId ), label, ( route, itemSubroute ) ) =
            if visible then
                Just
                    { label = label
                    , url = Routes.toUrl appState route
                    , active = activeSection == SharedSection itemSubroute
                    , testId = Just ("user_nav_" ++ testId)
                    }

            else
                Nothing

        isOwn =
            UuidOrCurrent.isCurrent uuidOrCurrent
                || Maybe.unwrap False (UuidOrCurrent.matchUuid uuidOrCurrent << .uuid) appState.config.user

        clientItem link =
            if not link.ownOnly || isOwn then
                Just
                    { label = link.label
                    , url = Routes.toUrl appState (Routes.usersEdit uuidOrCurrent) ++ "/" ++ link.path
                    , active = activeSection == ClientSection link.id
                    , testId = Just ("user_nav_" ++ link.id)
                    }

            else
                Nothing

        userSettingsItems =
            List.filterMap navItem
                [ ( ( True, "profile" )
                  , gettext "Profile" appState.locale
                  , ( Routes.usersEdit uuidOrCurrent, UserEditRoute.Profile )
                  )
                , ( ( True, "password" )
                  , gettext "Password" appState.locale
                  , ( Routes.usersEditPassword uuidOrCurrent, UserEditRoute.Password )
                  )
                , ( ( Feature.userEditConnectedAccounts appState uuidOrCurrent, "connected-accounts" )
                  , gettext "Connected Accounts" appState.locale
                  , ( Routes.usersEditConnectedAccounts, UserEditRoute.ConnectedAccounts )
                  )
                , ( ( Feature.userEditLanguage appState uuidOrCurrent, "language" )
                  , gettext "Language" appState.locale
                  , ( Routes.usersEditLanguage uuidOrCurrent, UserEditRoute.Language )
                  )
                , ( ( appState.config.features.toursEnabled && Feature.userEditTours appState uuidOrCurrent, "tours" )
                  , gettext "Tours" appState.locale
                  , ( Routes.usersEditTours uuidOrCurrent, UserEditRoute.Tours )
                  )
                , ( ( Feature.userEditApiKeys appState uuidOrCurrent, "api-keys" )
                  , gettext "API Keys" appState.locale
                  , ( Routes.usersEditApiKeys uuidOrCurrent, UserEditRoute.ApiKeys )
                  )
                , ( ( Feature.userEditActiveSessions appState uuidOrCurrent, "active-sessions" )
                  , gettext "Active Sessions" appState.locale
                  , ( Routes.usersEditActiveSessions uuidOrCurrent, UserEditRoute.ActiveSessions )
                  )
                , ( ( Feature.userEditSubmissionSettings appState uuidOrCurrent, "submission-settings" )
                  , gettext "Submission Settings" appState.locale
                  , ( Routes.usersEditSubmissionSettings uuidOrCurrent, UserEditRoute.SubmissionSettings )
                  )
                ]
                ++ List.filterMap clientItem appState.userSettingsLinks

        pluginItems =
            if Feature.userEditPlugins appState uuidOrCurrent then
                AppState.getPlugins appState
                    |> List.filter (Maybe.isJust << .userSettings << .connectors)
                    |> List.map
                        (\plugin ->
                            { label = plugin.name
                            , url = Routes.toUrl appState (Routes.usersEditPluginSettings uuidOrCurrent plugin.uuid)
                            , active = activeSection == SharedSection (UserEditRoute.PluginSettings plugin.uuid)
                            , testId = Just ("user_nav_plugin_" ++ Uuid.toString plugin.uuid)
                            }
                        )

            else
                []
    in
    -- No label over the sections themselves: it would only repeat "User
    -- Settings" from the top of the menu. The plugins are a set apart and keep
    -- theirs.
    [ ( Nothing, userSettingsItems )
    , ( Just (gettext "Plugins" appState.locale), pluginItems )
    ]


{-| What the page is about, by name, for the window's title, once it has
arrived.
-}
entityName : Model -> Maybe String
entityName model =
    Maybe.map User.fullName (ActionResult.toMaybe model.user)
