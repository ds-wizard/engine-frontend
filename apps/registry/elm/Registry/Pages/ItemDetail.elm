module Registry.Pages.ItemDetail exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

{-| One version of what the registry publishes: its readme, and beside it what
a wizard needs to import it — the `id:version` to paste, with a button that
copies it — and the other versions.

No publisher is shown: the registry has none.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Copyable as Copyable
import Common.Components.DetailPage as DetailPage
import Common.Components.Markdown as Markdown
import Common.Components.Page as Page
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html, li, ul)
import Registry.Api as Api
import Registry.Api.Models.Item as Item exposing (ItemDetail, Kind)
import Registry.Data.AppState exposing (AppState)
import Registry.Data.Kind as Kind
import Registry.Routes as Routes
import Ui.Badge as Badge
import Ui.Copy as Copy
import Ui.DescriptionList as DescriptionList
import Ui.Link as Link
import Ui.Resizer as Resizer
import Ui.Text as Text
import Version


type alias Model =
    { kind : Kind
    , reference : String
    , item : ActionResult ItemDetail
    , copy : Copy.Model
    , resizer : Resizer.Model
    }


init : AppState -> Kind -> String -> ( Model, Cmd Msg )
init appState kind reference =
    ( { kind = kind
      , reference = reference
      , item = Loading
      , copy = Copy.init
      , resizer = Resizer.init (DetailPage.resizerConfig appState)
      }
    , Api.getItem appState kind reference GetItemCompleted
    )


type Msg
    = GetItemCompleted (Result ApiError ItemDetail)
    | CopyMsg Copy.Msg
    | ResizerMsg Resizer.Msg


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetItemCompleted result ->
            case result of
                Ok item ->
                    ( { model | item = Success item }, Cmd.none )

                Err error ->
                    if ApiError.isNotFound error then
                        ( { model | item = Error (gettext "There is nothing published under this identifier." appState.locale) }, Cmd.none )

                    else
                        ( { model | item = ApiError.toActionResult appState (gettext "Unable to get it." appState.locale) error }, Cmd.none )

        CopyMsg copyMsg ->
            let
                ( copy, cmd ) =
                    Copy.update copyMsg model.copy
            in
            ( { model | copy = copy }, Cmd.map CopyMsg cmd )

        ResizerMsg resizerMsg ->
            ( { model | resizer = Resizer.update resizerMsg model.resizer }, Cmd.none )


{-| Dragging the side panel's edge, which follows the pointer across the
whole window.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ResizerMsg (Resizer.subscriptions model.resizer)


view : AppState -> Model -> Html Msg
view appState model =
    case model.item of
        Error message ->
            Page.error appState message

        _ ->
            DetailPage.view appState (viewItem appState model) model.item


viewItem : AppState -> Model -> ItemDetail -> DetailPage.DetailPage Msg
viewItem appState model item =
    let
        reference =
            Item.reference item

        versionLink version =
            if version == item.version then
                Text.body (Version.toString version ++ " " ++ gettext "(this version)" appState.locale)

            else
                Link.link
                    { label = Version.toString version
                    , url = Routes.toUrl (Routes.ItemDetail model.kind (Item.reference { id = item.id, version = version }))
                    }
                    |> Link.toHtml

        optionalEntry term mbValue =
            case mbValue of
                Just value ->
                    [ DescriptionList.entry { term = term, description = value } ]

                Nothing ->
                    []

        forkOfEntry =
            case item.forkOf of
                Just forkOf ->
                    [ DescriptionList.withHtml
                        { term = gettext "Fork of" appState.locale
                        , description =
                            [ Link.link
                                { label = Item.reference forkOf
                                , url = Routes.toUrl (Routes.ItemDetail model.kind (Item.reference forkOf))
                                }
                                |> Link.toHtml
                            ]
                        }
                    ]

                Nothing ->
                    []

        identifierSection =
            DetailPage.sectionHtml (Kind.idLabel appState model.kind)
                [ Copyable.inline appState
                    { key = "reference"
                    , label = gettext "Copy the identifier" appState.locale
                    , value = reference
                    , model = model.copy
                    , toMsg = CopyMsg
                    }
                , Text.bodySmall (gettext "Paste it in the import from the registry in your wizard." appState.locale)
                ]

        detailsSection =
            DetailPage.section (gettext "Details" appState.locale)
                ([ DescriptionList.entry { term = gettext "Version" appState.locale, description = Version.toString item.version }
                 , DescriptionList.entry { term = gettext "License" appState.locale, description = item.license }
                 ]
                    ++ optionalEntry (gettext "Metamodel version" appState.locale) item.metamodelVersion
                    ++ forkOfEntry
                    ++ [ DescriptionList.entry { term = gettext "Published" appState.locale, description = TimeUtils.toReadableDate appState.timeZone item.createdAt } ]
                )

        versionsSection =
            DetailPage.sectionHtml (gettext "Other versions" appState.locale)
                (if List.length item.versions > 1 then
                    [ ul [] (List.map (\version -> li [] [ versionLink version ]) (List.sortWith (\a b -> Version.compare b a) item.versions)) ]

                 else
                    []
                )
    in
    DetailPage.detailPage
        { title = item.name
        , content = [ Markdown.view { headingOffset = 1 } item.readme ]
        }
        |> DetailPage.withBadges [ Badge.neutral (Version.toString item.version) ]
        |> DetailPage.withSidePanel
            { resizer = model.resizer
            , toMsg = ResizerMsg
            , sections = [ identifierSection, detailsSection, versionsSection ]
            }
