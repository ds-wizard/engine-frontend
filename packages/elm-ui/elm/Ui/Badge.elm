module Ui.Badge exposing
    ( Badge
    , danger
    , info
    , inverse
    , neutral
    , private
    , success
    , toHtml
    , toLink
    , warning
    , withAction
    , withDescription
    , withIcon
    , withTestId
    )

{-| Opaque, so no caller can reach the class string: a badge that handed out its
classes would let any stylesheet's vocabulary leak straight through the
abstraction.
-}

import Html exposing (Html, a, button, span, text)
import Html.Attributes exposing (attribute, class, href, title, type_)
import Html.Events exposing (onClick)
import Ui.Icon as Icon
import Ui.TestId as TestId


type Badge msg
    = Badge
        { modifier : String
        , label : String
        , description : Maybe String
        , icon : Maybe Icon.Icon
        , action : Maybe msg
        , testId : Maybe String
        }


badge : String -> String -> Badge msg
badge modifier label =
    Badge { modifier = modifier, label = label, description = Nothing, icon = Nothing, action = Nothing, testId = Nothing }


neutral : String -> Badge msg
neutral =
    badge ""


success : String -> Badge msg
success =
    badge "ui-badge--success"


warning : String -> Badge msg
warning =
    badge "ui-badge--warning"


danger : String -> Badge msg
danger =
    badge "ui-badge--danger"


info : String -> Badge msg
info =
    badge "ui-badge--info"


{-| Purple, the private role's: not a status — nothing has gone right or wrong —
but a thing that is from somewhere else, or seen only by some: a row in a
workspace that belongs to the organization's library.
-}
private : String -> Badge msg
private =
    badge "ui-badge--private"


{-| The page's ink and ground, swapped: near-black with white text, and the
other way round on the dark theme.

For a badge that has to hold its shape whatever it is drawn on — an identifier
in a list of suggestions, where the row under the pointer takes a ground the
neutral badge's own is within a couple of per cent of. It says nothing about
state, which is what the colored ones are for; it is the neutral badge, made
unmissable.

-}
inverse : String -> Badge msg
inverse =
    badge "ui-badge--inverse"


{-| A longer explanation of what the badge means — "outdated" becomes "A newer
version of this knowledge model is available".

It is there for everyone, not only for a pointer. The sentence is inside the
badge as visually hidden text, so a screen reader reads it straight after the
label as part of the same thing, and it is also the badge's `title`, so hovering
shows it. A tooltip would have been the wrong tool: a badge is not interactive,
so it cannot be focused, and a tooltip on it can only ever be reached with a
mouse. `title` has the same limit, which is why it is the second copy and never
the only one — nothing is only in a tooltip.

Keep the label able to stand on its own; the description adds to it rather than
explaining a code.

-}
withDescription : String -> Badge msg -> Badge msg
withDescription description (Badge config) =
    Badge { config | description = Just description }


{-| Something the badge does — "update available" opening the dialog that does
the update. A real button, so it is focusable and announced as one, named by the
label and the description together.

It is for a badge whose whole meaning is that something can be done about it. A
badge that only reports a state has nothing to press and takes none of this; one
that goes somewhere is `toLink`, because navigation is a link.

-}
withAction : msg -> Badge msg -> Badge msg
withAction msg (Badge config) =
    Badge { config | action = Just msg }


{-| A picture before the label — a tick on "Resolved".

The label still says it in words: the icon is `aria-hidden`, because a badge
that needed its picture read out would be a badge whose words were not enough.

-}
withIcon : Icon.Icon -> Badge msg -> Badge msg
withIcon icon (Badge config) =
    Badge { config | icon = Just icon }


{-| The test hook on the badge's own element (`data-cy`), so a test can find
"deprecated" on a row without matching its words.
-}
withTestId : String -> Badge msg -> Badge msg
withTestId testId (Badge config) =
    Badge { config | testId = Just testId }


{-| The badge as it usually is: a word about the thing beside it, and nothing to
do. A badge given something to do (`withAction`) is drawn as the button it is.
-}
toHtml : Badge msg -> Html msg
toHtml (Badge config) =
    case config.action of
        Just msg ->
            view button
                [ type_ "button", class "ui-badge--action", onClick msg ]
                (Badge config)

        Nothing ->
            view span [] (Badge config)


{-| A badge that goes somewhere — "Outdated KM" to the settings where the
knowledge model is updated. A real link, so it is focusable and announced as
one, named by the label and the description together. It looks like the badge
it is, with an underline on hover to say it can be followed.
-}
toLink : String -> Badge msg -> Html msg
toLink url =
    view a [ href url, class "ui-badge--link" ]


view : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> Badge msg -> Html msg
view element attributes (Badge config) =
    let
        hooked =
            attributes ++ TestId.attributes config.testId

        picture =
            case config.icon of
                Just icon ->
                    [ span [ attribute "aria-hidden" "true" ] [ Icon.toHtml icon ] ]

                Nothing ->
                    []
    in
    case config.description of
        Just description ->
            element (class ("ui-badge " ++ config.modifier) :: title description :: hooked)
                (picture
                    ++ [ text config.label
                       , span [ class "ui-visually-hidden" ] [ text (": " ++ description) ]
                       ]
                )

        Nothing ->
            element (class ("ui-badge " ++ config.modifier) :: hooked) (picture ++ [ text config.label ])
