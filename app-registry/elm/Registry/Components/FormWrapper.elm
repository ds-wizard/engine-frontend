module Registry.Components.FormWrapper exposing
    ( ViewProps
    , view
    )

import Common.Utils.ShortcutUtils as Shortcut
import Html exposing (Html, form, h5, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Registry.Data.AppState exposing (AppState)
import Shortcut


type alias ViewProps msg =
    { title : String
    , submitMsg : msg
    , content : List (Html msg)
    }


view : AppState -> ViewProps msg -> Html msg
view appState props =
    Shortcut.shortcutElement [ Shortcut.submitShortcut appState.navigator.isMac props.submitMsg ]
        [ class "d-flex justify-content-center align-items-center my-5" ]
        [ form
            [ class "bg-white rounded shadow-sm p-4 w-100 box"
            , onSubmit props.submitMsg
            ]
            (h5 [] [ text props.title ] :: props.content)
        ]
