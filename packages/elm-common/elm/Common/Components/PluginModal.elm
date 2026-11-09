module Common.Components.PluginModal exposing
    ( Model
    , Msg
    , PluginModalState
    , ViewConfig
    , initialModel
    , open
    , update
    , view
    , viewWithId
    )

{-| A dialog that hosts a plugin's element, for connectors that act on something
(a document action, for instance).

**The dialog is the plugin's, all of it.** What the action is called, what it
says and which buttons it offers are the plugin's to render — it knows what it is
for and the application does not — so this draws no heading and no footer of its
own (`Ui.Modal.withContentOnly`). The way out is the plugin's too, through
`onActionClose`; Escape and the backdrop still work.

The dialog is named after the plugin all the same, because a dialog without a
name is announced as "dialog" and nothing else.

-}

import Common.Components.PluginView as PluginView
import Common.Data.AppState as AppState exposing (AppState)
import Common.Plugins.PluginElement as PluginElement exposing (PluginElement)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Ui.Modal as Modal
import Uuid exposing (Uuid)


type alias Model a =
    { state : Maybe (PluginModalState a)
    }


type alias PluginModalState a =
    { pluginUuid : Uuid
    , pluginElement : PluginElement
    , data : a
    }


initialModel : Model a
initialModel =
    { state = Nothing }


type Msg a
    = Open (PluginModalState a)
    | Close


open : PluginModalState a -> Msg a
open =
    Open


update : Msg a -> Model a -> Model a
update msg model =
    case msg of
        Open state ->
            { model | state = Just state }

        Close ->
            { model | state = Nothing }


type alias ViewConfig a msg =
    { attributes : a -> List (Html.Attribute msg)
    , wrapMsg : Msg a -> msg
    }


view : AppState -> ViewConfig a msg -> Model a -> Html msg
view appState cfg model =
    viewDialog appState Nothing cfg model


{-| `view`, for a page with more than one: the dialog's title is the plugin's
name, or "Plugin" while closed, so two of them would share their title's id.
-}
viewWithId : String -> AppState -> ViewConfig a msg -> Model a -> Html msg
viewWithId id appState cfg model =
    viewDialog appState (Just id) cfg model


viewDialog : AppState -> Maybe String -> ViewConfig a msg -> Model a -> Html msg
viewDialog appState mbId cfg model =
    let
        ( title, content ) =
            case model.state of
                Just state ->
                    ( AppState.getPlugin appState state.pluginUuid
                        |> Maybe.map .name
                        |> Maybe.withDefault (gettext "Plugin" appState.locale)
                    , [ PluginView.view appState
                            state.pluginUuid
                            state.pluginElement
                            (PluginElement.onActionClose (cfg.wrapMsg Close)
                                :: cfg.attributes state.data
                            )
                      ]
                    )

                Nothing ->
                    ( gettext "Plugin" appState.locale, [] )
    in
    Modal.plain
        { title = title
        , open = Maybe.isJust model.state
        , onClose = cfg.wrapMsg Close

        -- The plugin's element and nothing round it: the dialog's own wrapper
        -- is what keeps its child count stable, so a second one here would only
        -- put another box between the dialog's column and the plugin's.
        , body = content
        }
        |> Modal.withWide
        |> Modal.withContentOnly
        |> Modal.withTestId "plugin"
        |> (case mbId of
                Just id ->
                    Modal.withId id

                Nothing ->
                    identity
           )
        |> Modal.toHtml
