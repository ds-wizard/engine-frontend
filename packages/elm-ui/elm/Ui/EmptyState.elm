module Ui.EmptyState exposing
    ( EmptyState
    , emptyState
    , toHtml
    , withAction
    , withHeadingLevel
    , withIllustration
    , withTestId
    )

{-| Nothing to show, inside a page: a listing with no items yet, a search with
no results, a panel with nothing in it.

`Ui.Page.Message` is the full-page version and owns the page's only `h1`. This
one sits inside a page that already has its heading, so its own is an `h2` by
default and can be moved down the outline with `withHeadingLevel` when the empty
state is inside a section that has an `h2` of its own. The level changes the
element and not the size: how the heading looks belongs to the component, and
where it sits in the outline belongs to the page. Levels outside 2–6 are
clamped — a second `h1` would compete with the page's, and there is no `h7`.

The illustration is decorative and hidden from assistive technology. The heading
and the description already say everything the picture does, and an image with
no useful alternative text is read out as a file name or as nothing at all.

It is not a live region. When a search empties a listing, the listing's status
(`Ui.Page.List.withStatus`) is what announces "No results" — one announcement,
from the place that announces every other change to the count.

-}

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (attribute, class)
import Ui.Button as Button exposing (Button)
import Ui.TestId as TestId


type EmptyState msg
    = EmptyState
        { title : String
        , description : String
        , illustration : Maybe (Html msg)
        , action : Maybe (Button msg)
        , headingLevel : Int
        , testId : Maybe String
        }


emptyState : { title : String, description : String } -> EmptyState msg
emptyState config =
    EmptyState
        { title = config.title
        , description = config.description
        , illustration = Nothing
        , action = Nothing
        , headingLevel = 2
        , testId = Nothing
        }


withIllustration : Html msg -> EmptyState msg -> EmptyState msg
withIllustration illustration (EmptyState config) =
    EmptyState { config | illustration = Just illustration }


{-| The one thing to do about it — "Create a project". One, because an empty
state offering a choice of actions is a page that has not decided what it is
for.
-}
withAction : Button msg -> EmptyState msg -> EmptyState msg
withAction action (EmptyState config) =
    EmptyState { config | action = Just action }


withHeadingLevel : Int -> EmptyState msg -> EmptyState msg
withHeadingLevel level (EmptyState config) =
    EmptyState { config | headingLevel = clamp 2 6 level }


{-| The test hook, so a test can tell that a list is empty — after deleting its
last item, say — without matching the words.
-}
withTestId : String -> EmptyState msg -> EmptyState msg
withTestId testId (EmptyState config) =
    EmptyState { config | testId = Just testId }


toHtml : EmptyState msg -> Html msg
toHtml (EmptyState config) =
    div (class "ui-empty-state" :: TestId.attributes config.testId)
        [ case config.illustration of
            Just illustration ->
                div
                    [ class "ui-empty-state__illustration"
                    , attribute "aria-hidden" "true"
                    ]
                    [ illustration ]

            Nothing ->
                text ""
        , div [ class "ui-empty-state__text" ]
            [ Html.node ("h" ++ String.fromInt config.headingLevel)
                [ class "ui-empty-state__title" ]
                [ text config.title ]
            , p [ class "ui-empty-state__description" ] [ text config.description ]
            ]
        , case config.action of
            Just action ->
                div [ class "ui-empty-state__action" ] [ Button.toHtml action ]

            Nothing ->
                text ""
        ]
