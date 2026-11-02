module Common.Components.ImportPage exposing
    ( focusTab
    , view
    )

{-| The shape every import page shares: a title, and the ways in — from a file,
from the registry — as tabs when there is more than one.

The tabs are `Ui.Tabs`, not `Ui.NavTabs`. Switching source swaps the form in place
and leaves the address alone, and that is what a tablist is
for: one tab stop, arrows between sources, and a `tabpanel` the active tab
controls. Links would announce navigation that never happens.

With a single source there are no tabs at all. A tablist of one is a control
with nothing to choose, so the page is the title and the content.

    ImportPage.view appState
        { title = gettext "Import Knowledge Model" appState.locale
        , actions = [ guideLinkButton ]
        , tabs =
            [ { id = "registry", label = gettext "From registry" appState.locale, content = registryImport }
            , { id = "file", label = gettext "From file" appState.locale, content = fileImport }
            ]
        , active = model.source
        , onSelect = SourceSelected
        }

**Focus follows the arrow keys only if the page lets it.** An arrow key selects
the next source, and the re-render cannot move focus by itself, so return
`focusTab` when the selection arrives — otherwise focus stays on the tab that was
selected before, and Space selects it again:

    SourceSelected source ->
        ( { model | source = source }, ImportPage.focusTab NoOp source )

-}

import Common.Data.Context exposing (WithLocale)
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Ui.Button exposing (Button)
import Ui.Layout as Layout
import Ui.Page.Content as Content
import Ui.Tabs as Tabs


type alias Source msg =
    { id : String
    , label : String
    , content : List (Html msg)
    }


view :
    WithLocale a
    ->
        { title : String
        , actions : List (Button msg)
        , tabs : List (Source msg)
        , active : String
        , onSelect : String -> msg
        }
    -> Html msg
view appState config =
    let
        body =
            case config.tabs of
                [ only ] ->
                    only.content

                _ ->
                    let
                        activeSource =
                            config.tabs
                                |> List.find (\source -> source.id == config.active)
                                |> Maybe.withDefault (Maybe.withDefault { id = "", label = "", content = [] } (List.head config.tabs))
                    in
                    [ Layout.stack
                        [ Tabs.toHtmlWithTabIds identity
                            { id = tabsId
                            , label = gettext "Import from" appState.locale
                            , active = activeSource.id
                            , tabs = List.map (\source -> Tabs.tab { id = source.id, label = source.label, onSelect = config.onSelect source.id }) config.tabs
                            , full = False
                            }
                        , Tabs.panel { id = tabsId, label = activeSource.label } activeSource.content
                        ]
                    ]
    in
    Content.content { title = config.title, body = body }
        |> Content.withActions config.actions
        |> Content.narrow
        |> Content.toHtml


{-| Move focus to the tab of the source just selected. Return it from `update`
when `onSelect`'s message arrives — see the module documentation. `noop` is the
message sent once focus has moved, or when there was no such tab.
-}
focusTab : msg -> String -> Cmd msg
focusTab noop source =
    Tabs.focusTab noop tabsId source


tabsId : String
tabsId =
    "import-source"
