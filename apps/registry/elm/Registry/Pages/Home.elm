module Registry.Pages.Home exposing (view)

{-| What the registry has, one tile for each kind of thing it publishes.
-}

import Gettext exposing (gettext)
import Html exposing (Html)
import Registry.Api.Models.Item exposing (Kind)
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
        { title = gettext "Registry" appState.locale
        , body = [ ListItem.tiles (List.map (tile appState) (Kind.all appState)) ]
        }
        |> Content.withDescription (gettext "Customize your experience with prepared content and translations." appState.locale)
        |> Content.toHtml


tile : AppState -> Kind -> ListItem.ListItem msg
tile appState kind =
    ListItem.item
        (Link.link { label = Kind.title appState kind, url = Routes.toUrl (Routes.Items kind) }
            |> Link.toHtml
        )
        |> ListItem.withRowLink
        |> ListItem.withIcon (ItemIcon.toHtml (ItemIcon.withGlyph (Kind.icon kind) (ItemIcon.view (Kind.title appState kind))))
        |> ListItem.withDescription (Kind.description appState kind)
        |> ListItem.withTestId ("kinds_item_" ++ Routes.kindSegment kind)
