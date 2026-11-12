module Common.Pages.Dashboard exposing
    ( Model
    , Msg
    , init
    , initWithPlansStep
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    , viewWith
    )

{-| Dashboard: the feature router.

This feature has no nested route type and its top-level route has no arguments,
so the functions that other routers give a route take none here.

A greeting with the user's name as the page title, the announcements, and the
widgets the user's permissions allow, in the wizard's order. The widgets that
list something fetch it on `init`. It starts the dashboard tour.

-}

import Common.Api.Models.Announcement exposing (Announcement)
import Common.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Common.Components.Announcements as Announcements
import Common.Components.Tour as Tour
import Common.Data.AppState exposing (AppState)
import Common.Data.Session as Session
import Common.Data.TourId as TourId
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Dashboard.Widgets.AssignedComments as AssignedComments
import Common.Pages.Dashboard.Widgets.CallToAction as CallToAction
import Common.Pages.Dashboard.Widgets.OutdatedPackages as OutdatedPackages
import Common.Pages.Dashboard.Widgets.OutdatedTemplates as OutdatedTemplates
import Common.Pages.Dashboard.Widgets.RecentProjects as RecentProjects
import Common.Pages.Dashboard.Widgets.Usage as Usage
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extensions as List
import Maybe.Extra as Maybe
import Random
import String.Format as String
import Ui.Page.Dashboard as Dashboard
import Ui.Widget exposing (Widget)



-- MODEL


type alias Model =
    { assignedComments : AssignedComments.Model
    , outdatedPackages : OutdatedPackages.Model
    , outdatedTemplates : OutdatedTemplates.Model
    , recentProjects : RecentProjects.Model
    , usage : Usage.Model
    }


initialModel : Model
initialModel =
    { assignedComments = AssignedComments.initialModel
    , outdatedPackages = OutdatedPackages.initialModel
    , outdatedTemplates = OutdatedTemplates.initialModel
    , recentProjects = RecentProjects.initialModel
    , usage = Usage.initialModel
    }


init : AppState -> Model -> ( Model, Cmd Msg )
init appState =
    initWithPlansStep (projectsStep appState) appState


{-| `init`, with the tour's step to where the reader's plans are given by the
application, for where they are not the navigation's Projects. `init`'s is.
-}
initWithPlansStep : Tour.Step -> AppState -> Model -> ( Model, Cmd Msg )
initWithPlansStep plansStep =
    \appState _ ->
        let
            model =
                initialModel

            fetchCmd =
                []
                    |> List.insertIf (Cmd.map AssignedCommentsMsg (AssignedComments.fetchData appState)) True
                    |> List.insertIf (Cmd.map OutdatedPackagesMsg (OutdatedPackages.fetchData appState)) (OutdatedPackages.enabled appState)
                    |> List.insertIf (Cmd.map OutdatedTemplatesMsg (OutdatedTemplates.fetchData appState)) (OutdatedTemplates.enabled appState)
                    |> List.insertIf (Cmd.map RecentProjectsMsg (RecentProjects.fetchData appState)) True
                    |> List.insertIf (Cmd.map UsageMsg (Usage.fetchData appState)) (Usage.enabled appState)
                    |> Cmd.batch
        in
        ( model, Cmd.batch [ fetchCmd, tour appState plansStep ] )


projectsStep : AppState -> Tour.Step
projectsStep appState =
    { element = Just "#menu_projects"
    , title = gettext "Projects" appState.locale
    , description = gettext "Create and manage your data management plans here." appState.locale
    }


tour : AppState -> Tour.Step -> Cmd msg
tour appState plansStep =
    let
        firstStep =
            { element = Tour.centered
            , title = String.format (gettext "Welcome to %s" appState.locale) [ LookAndFeelConfig.getAppTitle appState.config.lookAndFeel ]
            , description = gettext "We'll guide you through creating your data management plan." appState.locale
            }
    in
    Tour.tour appState
        { tourId = TourId.dashboard
        , completedTours = appState.config.tours
        , enabled = appState.config.features.toursEnabled && Session.exists appState.session
        , steps = [ firstStep, plansStep ]
        }



-- UPDATE


type Msg
    = AssignedCommentsMsg AssignedComments.Msg
    | OutdatedPackagesMsg OutdatedPackages.Msg
    | OutdatedTemplatesMsg OutdatedTemplates.Msg
    | RecentProjectsMsg RecentProjects.Msg
    | UsageMsg Usage.Msg


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    let
        ( newModel, cmd ) =
            case msg of
                AssignedCommentsMsg widgetMsg ->
                    let
                        ( widgetModel, widgetCmd ) =
                            AssignedComments.update (UpdateConfig.map AssignedCommentsMsg cfg) appState widgetMsg model.assignedComments
                    in
                    ( { model | assignedComments = widgetModel }, widgetCmd )

                OutdatedPackagesMsg widgetMsg ->
                    let
                        ( widgetModel, widgetCmd ) =
                            OutdatedPackages.update (UpdateConfig.map OutdatedPackagesMsg cfg) appState widgetMsg model.outdatedPackages
                    in
                    ( { model | outdatedPackages = widgetModel }, widgetCmd )

                OutdatedTemplatesMsg widgetMsg ->
                    let
                        ( widgetModel, widgetCmd ) =
                            OutdatedTemplates.update (UpdateConfig.map OutdatedTemplatesMsg cfg) appState widgetMsg model.outdatedTemplates
                    in
                    ( { model | outdatedTemplates = widgetModel }, widgetCmd )

                RecentProjectsMsg widgetMsg ->
                    let
                        ( widgetModel, widgetCmd ) =
                            RecentProjects.update (UpdateConfig.map RecentProjectsMsg cfg) appState widgetMsg model.recentProjects
                    in
                    ( { model | recentProjects = widgetModel }, widgetCmd )

                UsageMsg widgetMsg ->
                    let
                        ( widgetModel, widgetCmd ) =
                            Usage.update (UpdateConfig.map UsageMsg cfg) appState widgetMsg model.usage
                    in
                    ( { model | usage = widgetModel }, widgetCmd )
    in
    ( appState.seed, newModel, cmd )


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    \_ _ ->
        Sub.none



-- VIEW


view : AppState -> Model -> Html Msg
view =
    viewWith []


{-| The dashboard with the application's own widgets first, for what the
application keeps itself: they come before the shared ones, and are drawn as
they are given.
-}
viewWith : List (Widget msg) -> AppState -> Model -> Html msg
viewWith leading appState model =
    let
        title =
            case appState.config.user of
                Just user ->
                    String.format (gettext "Welcome, %s!" appState.locale) [ user.firstName ]

                Nothing ->
                    gettext "Welcome!" appState.locale

        widgets =
            leading
                ++ Maybe.values
                    [ AssignedComments.view appState model.assignedComments
                    , Just (RecentProjects.view appState model.recentProjects)
                    , CallToAction.createProject appState
                    , CallToAction.createProjectTemplate appState
                    , CallToAction.createKnowledgeModel appState
                    , whenEnabled OutdatedPackages.enabled (OutdatedPackages.view appState model.outdatedPackages)
                    , whenEnabled OutdatedTemplates.enabled (OutdatedTemplates.view appState model.outdatedTemplates)
                    , CallToAction.importKnowledgeModel appState
                    , CallToAction.importDocumentTemplate appState
                    , whenEnabled Usage.enabled (Just (Usage.view appState model.usage))
                    , CallToAction.configureLookAndFeel appState
                    , CallToAction.connectRegistry appState
                    , CallToAction.addOpenId appState
                    ]

        whenEnabled enabled mbWidget =
            if enabled appState then
                mbWidget

            else
                Nothing
    in
    Dashboard.dashboard { title = title, widgets = widgets }
        |> Dashboard.withNotices (announcementNotices appState.config.dashboardAndMenu.announcements)
        |> Dashboard.toHtml


{-| The announcements the wizard shows above the dashboard, before the widgets.
-}
announcementNotices : List Announcement -> List (Html msg)
announcementNotices announcements =
    if List.isEmpty announcements then
        []

    else
        [ Announcements.view announcements ]


isGuarded : AppState -> Model -> Maybe String
isGuarded =
    \_ _ ->
        Nothing
