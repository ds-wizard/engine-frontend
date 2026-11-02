module Ui.Widget exposing
    ( Widget
    , full
    , toHtml
    , widget
    , withAction
    , withDescription
    )

{-| One box on a dashboard: a title, what it is about, and whatever it shows.

The size contract is the part of the existing `WidgetBox` worth keeping. A
widget is half the dashboard's width or all of it, and nothing in between: the
two-column grid is what makes a dashboard scannable, and a widget that could ask
for a third would make it a collage.

Each widget is a `section` named by its own `h2` through `aria-labelledby`. A
section with a name is a region, so the widgets are in the screen reader's list
of landmarks and a dashboard can be walked by them; a section without one is a
`div` to everything that reads out loud.

Rendered by `Ui.Page.Dashboard`, which gives each one its id.

-}

import Html exposing (Html, div, h2, header, p, section, text)
import Html.Attributes exposing (attribute, class, classList, id)
import Ui.Button as Button exposing (Button)


type Widget msg
    = Widget
        { title : String
        , body : List (Html msg)
        , full : Bool
        , action : Maybe (Button msg)
        , description : Maybe String
        }


{-| Half the dashboard's width, which is the default because it is what most of
them are. On a narrow page every widget is the full width regardless.
-}
widget : { title : String, body : List (Html msg) } -> Widget msg
widget config =
    Widget
        { title = config.title
        , body = config.body
        , full = False
        , action = Nothing
        , description = Nothing
        }


{-| The whole width of the dashboard — for a table or a chart that needs it.
-}
full : Widget msg -> Widget msg
full (Widget config) =
    Widget { config | full = True }


{-| One action in the header, at the end of the title's row — "View all".

One, because a widget is a summary of something that lives elsewhere, and the
way there is the action. A second one is usually a sign the widget is a page.
A link that navigates is `Button.link`, so it stays an anchor.

-}
withAction : Button msg -> Widget msg -> Widget msg
withAction action (Widget config) =
    Widget { config | action = Just action }


{-| A sentence under the title saying what is counted or listed — "Projects
changed in the last 30 days". It is part of reading the numbers, not a caption.
-}
withDescription : String -> Widget msg -> Widget msg
withDescription description (Widget config) =
    Widget { config | description = Just description }


{-| The id is the widget's own, unique on the page; the heading is named after
it. `Ui.Page.Dashboard` supplies it, so a dashboard's widgets cannot collide.
-}
toHtml : String -> Widget msg -> Html msg
toHtml widgetId (Widget config) =
    let
        headingId =
            widgetId ++ "-title"
    in
    section
        [ classList
            [ ( "ui-widget", True )
            , ( "ui-widget--full", config.full )
            ]
        , id widgetId
        , attribute "aria-labelledby" headingId
        ]
        [ header [ class "ui-widget__header" ]
            [ div [ class "ui-widget__heading" ]
                [ h2 [ class "ui-widget__title", id headingId ] [ text config.title ]
                , case config.description of
                    Just description ->
                        p [ class "ui-widget__description" ] [ text description ]

                    Nothing ->
                        text ""
                ]
            , case config.action of
                Just action ->
                    div [ class "ui-widget__action" ] [ Button.toHtml (Button.small action) ]

                Nothing ->
                    text ""
            ]
        , div [ class "ui-widget__body" ] config.body
        ]
