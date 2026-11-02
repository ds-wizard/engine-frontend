module Common.Pages.Projects.Components.ProjectActions exposing (actions)

{-| The actions of a project in the projects listing, grouped and gated as the
wizard's listing dropdown was.

"Settings" is a link to the project's settings tab, shown to those who may open
that tab. In the wizard the way there from the list was the "Outdated KM" and
"Outdated DT" badges, which were links; a badge here is not interactive, so the
link lives in the menu.

-}

import Common.Api.Models.Project exposing (Project)
import Common.Components.ItemActions as ItemActions
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Routes as Routes
import Gettext exposing (gettext)
import Ui.Icon as Icon


actions :
    AppState
    -> { clone : Project -> msg, delete : Project -> msg }
    -> Project
    -> List (ItemActions.Action msg)
actions appState msgs project =
    let
        openProject =
            ItemActions.link
                { icon = Icon.regular "fa-folder-open"
                , label = gettext "Open project" appState.locale
                , url = Routes.toUrl appState (Routes.projectsDetail project.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_open"

        createProjectFromTemplate =
            ItemActions.link
                { icon = Icon.regular "fa-rectangle-list"
                , label = gettext "Create project from this template" appState.locale
                , url = Routes.toUrl appState (Routes.projectsCreateFromProjectTemplate project.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_create-project-from-template"

        clone =
            ItemActions.action
                { icon = Icon.regular "fa-copy"
                , label = gettext "Clone" appState.locale
                , onSelect = msgs.clone project
                }
                |> ItemActions.withTestId "listing-item_action_clone"

        createMigration =
            ItemActions.link
                { icon = Icon.solid "fa-shuffle"
                , label = gettext "Migrate" appState.locale
                , url = Routes.toUrl appState (Routes.projectsCreateMigration project.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_create-migration"

        settings =
            ItemActions.link
                { icon = Icon.solid "fa-gears"
                , label = gettext "Settings" appState.locale
                , url = Routes.toUrl appState (Routes.projectsDetailSettings project.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_settings"

        delete =
            ItemActions.dangerAction
                { icon = Icon.solid "fa-trash"
                , label = gettext "Delete" appState.locale
                , onSelect = msgs.delete project
                }
                |> ItemActions.withTestId "listing-item_action_delete"
    in
    ItemActions.groups
        [ [ ( openProject, True ) ]
        , [ ( createProjectFromTemplate, Feature.projectCreateFromTemplate appState project ) ]
        , [ ( clone, Feature.projectsCreate appState )
          , ( createMigration, Feature.projectCreateMigration appState project )
          , ( settings, Feature.projectSettings appState project )
          ]
        , [ ( delete, Feature.projectDelete appState project ) ]
        ]
