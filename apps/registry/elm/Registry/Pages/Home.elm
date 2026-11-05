module Registry.Pages.Home exposing (view)

{-| What the registry has, one tile for each kind of thing it publishes.
-}

import Gettext exposing (gettext)
import Html exposing (Html)
import Registry.Data.AppState exposing (AppState)
import Registry.Data.Kind as Kind
import Registry.Routes as Routes
import Ui.ItemIcon as ItemIcon
import Ui.Link as Link
import Ui.ListItem as ListItem
import Ui.Page.Content as Content


view : AppState -> Html msg
view appState =
    Content.content
        { title = appState.appTitle
        , body = [ ListItem.tiles (List.map tile (Kind.sections appState)) ]
        }
        |> Content.withDescription (gettext "Customize your experience with prepared content and translations." appState.locale)
        |> Content.toHtml


tile : Kind.Section -> ListItem.ListItem msg
tile section =
    ListItem.item
        (Link.link { label = section.title, url = Routes.toUrl (Routes.Items section.kind) }
            |> Link.toHtml
        )
        |> ListItem.withRowLink
        |> ListItem.withIcon (ItemIcon.toHtml (ItemIcon.withGlyph section.icon (ItemIcon.view section.title)))
        |> ListItem.withDescription section.description
        |> ListItem.withTestId ("kinds_item_" ++ section.testId)
