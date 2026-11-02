module Ui.TestId exposing (attribute, attributes)

{-| The test hook: `data-cy`, the one attribute end-to-end tests select by
besides an element's `id`. Every component that takes a test id renders it
through here, so the convention has one spelling, and "does this component
take a test id" is a search for `Ui.TestId`.

A component that renders a list of the caller's items — rows, entries, options,
tabs, crumbs — takes a test id per item (a `withTestId` on the item, or a
function from the item to its test id), not only one for the whole component:
a test has to be able to find the row for one thing without matching its text.

-}

import Html
import Html.Attributes


attribute : String -> Html.Attribute msg
attribute =
    Html.Attributes.attribute "data-cy"


{-| The hook when there is one, for a component whose test id is optional.
-}
attributes : Maybe String -> List (Html.Attribute msg)
attributes mbTestId =
    case mbTestId of
        Just testId ->
            [ attribute testId ]

        Nothing ->
            []
