module Registry.Pages.Items exposing
    ( Listing
    , Model
    , Msg
    , init
    , update
    , view
    )

{-| What the registry publishes of one kind, with a field that narrows the list
as one types. The list is short enough to be sent whole, so narrowing it asks
the server nothing.

Knowledge models and templates come newest first, narrowed by name, id or
description, each linking to its page. Translations come by name, narrowed by
name or language code, each with its PO file to download; the two kinds are tabs
of one Translations page, Application and Emails, as in a wizard's settings.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Page as Page
import Common.Components.Timestamp as Timestamp
import Gettext exposing (gettext)
import Html exposing (Html)
import Registry.Api as Api
import Registry.Api.Models.Item as Item exposing (Item, Kind)
import Registry.Api.Models.Translation exposing (Translation)
import Registry.Data.AppState exposing (AppState)
import Registry.Data.Kind as Kind
import Registry.Routes as Routes
import String.Format as String
import Time
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.EmptyState as EmptyState
import Ui.Field as Field
import Ui.Illustration as Illustration
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.ListItem as ListItem
import Ui.NavTabs as NavTabs
import Ui.Page.List as ListPage
import Ui.Text as Text
import Version


type alias Model =
    { kind : Kind
    , items : ActionResult Listing
    , filter : String
    }


type Listing
    = Packages (List Item)
    | Translations (List Translation)


init : AppState -> Kind -> ( Model, Cmd Msg )
init appState kind =
    ( { kind = kind, items = Loading, filter = "" }
    , if Kind.isTranslation kind then
        Api.getTranslations appState kind (GetItemsCompleted << Result.map Translations)

      else
        Api.getItems appState kind (GetItemsCompleted << Result.map Packages)
    )


type Msg
    = GetItemsCompleted (Result ApiError Listing)
    | SetFilter String


update : AppState -> Msg -> Model -> Model
update appState msg model =
    case msg of
        GetItemsCompleted result ->
            case result of
                Ok (Packages items) ->
                    { model | items = Success (Packages (List.sortBy (negate << Time.posixToMillis << .createdAt) items)) }

                Ok (Translations translations) ->
                    { model | items = Success (Translations (List.sortBy (\t -> ( String.toLower t.name, t.code )) translations)) }

                Err error ->
                    { model | items = ApiError.toActionResult appState (Kind.unableToGet appState model.kind) error }

        SetFilter filter ->
            { model | filter = filter }


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (viewItems appState model) model.items


viewItems : AppState -> Model -> Listing -> Html Msg
viewItems appState model listing =
    case listing of
        Packages items ->
            viewList appState model (\item -> [ item.name, item.id, item.description ]) (viewItem appState model.kind) items

        Translations translations ->
            viewList appState model (\translation -> [ translation.name, translation.code ]) (viewTranslation appState model.kind) translations


viewList : AppState -> Model -> (a -> List String) -> (a -> ListItem.ListItem Msg) -> List a -> Html Msg
viewList appState model searched viewOne items =
    let
        needle =
            String.toLower (String.trim model.filter)

        matches item =
            List.any (String.contains needle << String.toLower) (searched item)

        filterField =
            Field.input
                { id = "registry-filter"
                , label = gettext "Search" appState.locale
                , value = model.filter
                , onInput = SetFilter
                }
                |> Field.withAutocomplete "off"
                |> Field.toHtml

        content =
            if List.isEmpty items then
                [ EmptyState.emptyState { title = Kind.nothingPublished appState model.kind, description = gettext "Come back later, or ask its administrators." appState.locale }
                    |> EmptyState.withIllustration (Illustration.toHtml Illustration.empty)
                    |> EmptyState.toHtml
                ]

            else
                let
                    shown =
                        List.filter matches items
                in
                if List.isEmpty shown then
                    [ Text.body (gettext "Nothing matches the search." appState.locale) ]

                else
                    [ ListItem.list (List.map viewOne shown) ]

        ( pageTitle, pageDescription, toolbar ) =
            if Kind.isTranslation model.kind then
                ( Kind.translationsTitle appState
                , Kind.translationsDescription appState
                , Layout.stack [ viewTranslationTabs appState model.kind, filterField ]
                )

            else
                ( Kind.title appState model.kind, Kind.description appState model.kind, filterField )
    in
    ListPage.list
        { title = pageTitle
        , toolbar = toolbar
        , content = content
        }
        |> ListPage.withDescription pageDescription
        -- Said when the list arrives, not on every keystroke of the search.
        |> ListPage.withStatus (Kind.count appState model.kind (List.length items))
        |> ListPage.toHtml


viewTranslationTabs : AppState -> Kind -> Html msg
viewTranslationTabs appState current =
    let
        tab kind =
            NavTabs.item
                { label = Kind.tabTitle appState kind
                , url = Routes.toUrl (Routes.Items kind)
                , active = kind == current
                }
                |> NavTabs.withTestId ("translations-tab_" ++ Routes.kindSegment kind)
    in
    NavTabs.toHtml
        { label = Kind.translationsTitle appState
        , items = List.map tab (Kind.translations appState)
        , full = False
        }


viewItem : AppState -> Kind -> Item -> ListItem.ListItem Msg
viewItem appState kind item =
    ListItem.item
        (Link.link { label = item.name, url = Routes.toUrl (Routes.ItemDetail kind (Item.reference item)) }
            |> Link.toHtml
        )
        |> ListItem.withRowLink
        |> ListItem.withFragments
            [ Text.code item.id
            , Badge.toHtml (Badge.neutral (Version.toString item.version))
            ]
        |> ListItem.withDescription item.description
        |> ListItem.withTimestamp (Timestamp.created appState item.createdAt)
        |> ListItem.withTestId ("items_item_" ++ Item.reference item)


viewTranslation : AppState -> Kind -> Translation -> ListItem.ListItem Msg
viewTranslation appState kind translation =
    ListItem.item (Html.text translation.name)
        |> ListItem.withTestId ("translations_item_" ++ translation.code)
        |> ListItem.withFragments
            [ Text.code translation.code
            , Badge.toHtml (Badge.neutral translation.version)
            ]
        |> ListItem.withActions
            (Button.link (gettext "Download PO" appState.locale) (Api.translationPoUrl appState kind translation)
                |> Button.small
                |> Button.withAccessibleName (String.format (gettext "Download the PO file of %s" appState.locale) [ translation.name ])
                |> Button.toHtml
            )
