module Common.Components.Questionnaire.Toolbar exposing
    ( Counts
    , Model
    , Msg
    , UpdateConfig
    , importerPlaceholder
    , init
    , subscriptions
    , update
    , viewEnd
    , viewStart
    )

{-| The questionnaire's toolbar: at the start, what the reader sees (the View
menu), what comes in (importers) and what goes out (project actions); at the
end, the panels, each a toggle with a count where one means something.

It is two halves because the workspace places them (`Ui.Page.Workspace`), and each
half is one lazy call over primitives — counts, not lists; the panel, which the
root keeps — so a keystroke that changes no count renders neither.

No fullscreen toggle: the questionnaire lives in the project page's frame like
every other tab.

-}

import Common.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Components.Questionnaire.RightPanel as RightPanel exposing (RightPanel)
import Common.Components.Questionnaire.ViewSettings as ViewSettings exposing (ViewSettings)
import Common.Data.AppState as AppState exposing (AppState)
import Common.Plugins.Plugin as Plugin exposing (Plugin, ProjectActionConnector, ProjectImporterConnector)
import Common.Plugins.PluginElement exposing (PluginElement)
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, span)
import Html.Attributes exposing (class)
import Html.Lazy as Lazy
import String.Format as String
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Menu as Menu
import Ui.Tooltip as Tooltip
import Uuid exposing (Uuid)


type alias Model =
    { projectUuid : Uuid
    , viewMenu : Menu.Model
    , importMenu : Menu.Model
    , actionsMenu : Menu.Model
    , actions : List ( Plugin, ProjectActionConnector )
    , importers : List ( Plugin, ProjectImporterConnector )
    }


init : AppState -> ProjectQuestionnaire -> Model
init appState questionnaire =
    let
        packageId =
            KnowledgeModelUtils.getPackageId questionnaire.knowledgeModelPackage
    in
    { projectUuid = questionnaire.uuid
    , viewMenu = Menu.init
    , importMenu = Menu.init
    , actionsMenu = Menu.init
    , actions =
        AppState.getPluginsByConnector appState .projectActions
            |> Plugin.filterByKmPatterns packageId
            |> List.sortBy (Tuple.second >> .name)
    , importers =
        AppState.getPluginsByConnector appState .projectImporters
            |> Plugin.filterByKmPatterns packageId
            |> List.sortBy (Tuple.second >> .name)
    }


type Msg
    = ViewMenuMsg Menu.Msg
    | ImportMenuMsg Menu.Msg
    | ActionsMenuMsg Menu.Msg
    | SetViewSettings ViewSettings
    | OpenAction Uuid PluginElement
    | TogglePanel RightPanel


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , setViewSettingsCmd : ViewSettings -> Cmd msg
    , openActionCmd : Uuid -> PluginElement -> Cmd msg
    , togglePanelCmd : RightPanel -> Cmd msg
    }


viewMenuId : String
viewMenuId =
    "questionnaire-view-menu"


importMenuId : String
importMenuId =
    "questionnaire-import-menu"


actionsMenuId : String
actionsMenuId =
    "questionnaire-actions-menu"


update : UpdateConfig msg -> Gettext.Locale -> ViewSettings -> Msg -> Model -> ( Model, Cmd msg )
update cfg locale viewSettings msg model =
    case msg of
        ViewMenuMsg menuMsg ->
            let
                ( menu, cmd ) =
                    Menu.update viewMenuId (Menu.focusableCount (viewItems locale viewSettings)) menuMsg model.viewMenu
            in
            ( { model | viewMenu = menu }, Cmd.map (cfg.wrapMsg << ViewMenuMsg) cmd )

        ImportMenuMsg menuMsg ->
            let
                ( menu, cmd ) =
                    Menu.update importMenuId (List.length model.importers) menuMsg model.importMenu
            in
            ( { model | importMenu = menu }, Cmd.map (cfg.wrapMsg << ImportMenuMsg) cmd )

        ActionsMenuMsg menuMsg ->
            let
                ( menu, cmd ) =
                    Menu.update actionsMenuId (List.length model.actions) menuMsg model.actionsMenu
            in
            ( { model | actionsMenu = menu }, Cmd.map (cfg.wrapMsg << ActionsMenuMsg) cmd )

        SetViewSettings settings ->
            -- The View menu stays open: its items are checks, and turning three
            -- of them off is three presses, not three trips to the menu.
            ( model, cfg.setViewSettingsCmd settings )

        OpenAction pluginUuid element ->
            let
                ( menu, cmd ) =
                    Menu.update actionsMenuId (List.length model.actions) Menu.closeWithoutFocus model.actionsMenu
            in
            ( { model | actionsMenu = menu }
            , Cmd.batch [ Cmd.map (cfg.wrapMsg << ActionsMenuMsg) cmd, cfg.openActionCmd pluginUuid element ]
            )

        TogglePanel panel ->
            ( model, cfg.togglePanelCmd panel )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ViewMenuMsg (Menu.subscriptions viewMenuId model.viewMenu)
        , Sub.map ImportMenuMsg (Menu.subscriptions importMenuId model.importMenu)
        , Sub.map ActionsMenuMsg (Menu.subscriptions actionsMenuId model.actionsMenu)
        ]



-- VIEW


{-| `importersEnabled` is the project's feature flag (`Feature.projectToolbarImporters`).
-}
viewStart : Gettext.Locale -> String -> ViewSettings -> Bool -> Model -> Html Msg
viewStart locale importUrl viewSettings importersEnabled model =
    Lazy.lazy5 viewStartLazy locale importUrl viewSettings importersEnabled model


viewStartLazy : Gettext.Locale -> String -> ViewSettings -> Bool -> Model -> Html Msg
viewStartLazy locale importUrl viewSettings importersEnabled model =
    let
        viewLabel =
            if ViewSettings.anyHidden viewSettings then
                gettext "View (some hidden)" locale

            else
                gettext "View" locale

        importMenu =
            if importersEnabled && not (List.isEmpty model.importers) then
                [ Menu.toHtml
                    { id = importMenuId
                    , trigger = Menu.quiet (gettext "Import" locale)
                    , alignment = Menu.Start
                    , items =
                        List.map
                            (\( _, connector ) -> Menu.linkItem connector.name (String.replace importerPlaceholder connector.url importUrl))
                            model.importers
                    , toMsg = ImportMenuMsg
                    }
                    model.importMenu
                ]

            else
                []

        actionsMenu =
            if List.isEmpty model.actions then
                []

            else
                [ Menu.toHtml
                    { id = actionsMenuId
                    , trigger = Menu.quiet (gettext "Actions" locale)
                    , alignment = Menu.Start
                    , items = List.map (\( plugin, connector ) -> Menu.item connector.name (OpenAction plugin.uuid connector.element)) model.actions
                    , toMsg = ActionsMenuMsg
                    }
                    model.actionsMenu
                ]
    in
    span [ class "c-questionnaire-toolbar__group" ]
        (Menu.toHtml
            { id = viewMenuId
            , trigger = Menu.quiet viewLabel
            , alignment = Menu.Start
            , items = viewItems locale viewSettings
            , toMsg = ViewMenuMsg
            }
            model.viewMenu
            :: importMenu
            ++ actionsMenu
        )


viewItems : Gettext.Locale -> ViewSettings -> List (Menu.Item Msg)
viewItems locale viewSettings =
    let
        check checked label toggle =
            Menu.checkItem checked label (SetViewSettings (toggle viewSettings))
    in
    [ Menu.item (gettext "Show all" locale) (SetViewSettings ViewSettings.all)
    , Menu.item (gettext "Hide all" locale) (SetViewSettings ViewSettings.none)
    , Menu.separator
    , check viewSettings.answeredBy (gettext "Answered by" locale) ViewSettings.toggleAnsweredBy
    , check viewSettings.phases (gettext "Phases" locale) ViewSettings.togglePhases
    , check viewSettings.tags (gettext "Question tags" locale) ViewSettings.toggleTags
    , check viewSettings.nonDesirableQuestions (gettext "Non-desirable questions" locale) ViewSettings.toggleNonDesirableQuestions
    , check viewSettings.metricValues (gettext "Metric values" locale) ViewSettings.toggleMetricValues
    ]


{-| What the end half shows: how many of each, and which panels the project
offers. All primitives, for the lazy call.
-}
type alias Counts =
    { warnings : Int
    , unanswered : Int
    , todos : Int
    , comments : Int
    }


viewEnd : Gettext.Locale -> RightPanel -> Counts -> Int -> Html Msg
viewEnd locale rightPanel counts features =
    Lazy.lazy7 viewEndLazy locale rightPanel counts.warnings counts.unanswered counts.todos counts.comments features


{-| `features` is a bitmask: 1 TODOs, 2 comments, 4 version history.
-}
viewEndLazy : Gettext.Locale -> RightPanel -> Int -> Int -> Int -> Int -> Int -> Html Msg
viewEndLazy locale rightPanel warnings unanswered todos comments features =
    let
        has bit =
            modBy (bit * 2) features >= bit

        toggle panel pressed label testId count description =
            let
                button =
                    Button.ghost label (TogglePanel panel)
                        |> Button.quiet
                        |> Button.small
                        |> Button.withPressed pressed
                        |> Button.withTestId testId
            in
            Button.toHtml
                (if count > 0 then
                    Button.withBadge (Badge.neutral (String.fromInt count) |> Badge.withDescription (description count)) button

                 else
                    button
                )

        plural singular pluralForm count =
            String.format (ngettext ( singular, pluralForm ) count locale) [ String.fromInt count ]
    in
    span [ class "c-questionnaire-toolbar__group" ]
        (List.concat
            [ if warnings > 0 || rightPanel == RightPanel.Warnings then
                [ toggle RightPanel.Warnings (rightPanel == RightPanel.Warnings) (gettext "Warnings" locale) "questionnaire_toolbar_warnings" warnings (plural "%s warning" "%s warnings") ]

              else
                []
            , [ toggle RightPanel.UnansweredQuestions (rightPanel == RightPanel.UnansweredQuestions) (gettext "Unanswered" locale) "questionnaire_toolbar_unanswered-questions" unanswered (plural "%s unanswered question" "%s unanswered questions") ]
            , if has 1 || rightPanel == RightPanel.TODOs then
                [ toggle RightPanel.TODOs (rightPanel == RightPanel.TODOs) (gettext "TODOs" locale) "questionnaire_toolbar_todos" todos (plural "%s TODO" "%s TODOs") ]

              else
                []
            , if has 2 then
                [ toggle RightPanel.CommentsOverview (RightPanel.isCommentsFamily rightPanel) (gettext "Comments" locale) "questionnaire_toolbar_comments" comments (plural "%s unresolved comment" "%s unresolved comments") ]

              else
                []
            , if has 4 then
                [ toggle RightPanel.VersionHistory (rightPanel == RightPanel.VersionHistory) (gettext "Version history" locale) "questionnaire-version-history" 0 (always "") ]

              else
                []
            , [ Button.toHtml
                    (Button.iconOnly (Icon.solid "fa-magnifying-glass") (gettext "Search" locale) (TogglePanel RightPanel.Search)
                        |> Button.quiet
                        |> Button.small
                        |> Button.withTooltipAt Tooltip.End
                        |> Button.withPressed (rightPanel == RightPanel.Search)
                        |> Button.withTestId "questionnaire-search"
                    )
              ]
            ]
        )


{-| Where an importer's own address goes in the import page's URL, which the
caller builds: the toolbar is drawn lazily, from plain values.
-}
importerPlaceholder : String
importerPlaceholder =
    "__importer__"
