module Registry2.Components.DetailPage exposing
    ( ViewProps
    , view
    )

import Gettext exposing (gettext)
import Html exposing (Html, div, hr, i, span, strong, text)
import Html.Attributes exposing (class)
import Registry2.Data.AppState exposing (AppState)
import Shared.Common.TimeUtils as TimeUtils
import Shared.Markdown as Markdown
import String.Format as String
import Time
import Version exposing (Version)


type alias ViewProps msg =
    { icon : String
    , title : String
    , version : Version
    , published : Time.Posix
    , readme : String
    , sidebar : List (Html msg)
    }


view : AppState -> ViewProps msg -> Html msg
view appState props =
    div []
        [ div [ class "row" ]
            [ div [ class "col-12 pb-3" ]
                [ div []
                    [ i [ class (props.icon ++ " me-2") ] []
                    , strong [] [ text props.title ]
                    ]
                , div [ class "publish-info mt-1 font-monospace" ]
                    [ span [ class "fragment" ] [ text (Version.toString props.version) ]
                    , span [ class "fragment" ]
                        [ text
                            (String.format (gettext "Published on %s" appState.locale)
                                [ TimeUtils.toReadableDate appState.timeZone props.published ]
                            )
                        ]
                    ]
                , hr [] []
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-12 col-lg-8 " ]
                [ div [ class "bg-white rounded shadow-sm" ]
                    [ div [ class "px-4 fs-sm pt-3 text-grey font-monospace" ]
                        [ i [ class "fas fa-file me-2" ]
                            []
                        , text (gettext "Readme" appState.locale)
                        ]
                    , Markdown.toHtml [ class "py-4 px-4" ] props.readme
                    ]
                ]
            , div [ class "col-12 col-lg-4 sidebar" ] props.sidebar
            ]
        ]
