module Common.Components.Questionnaire.Nest exposing (commentsButtonId, listItem, titleId)

{-| One node of the questionnaire's list: the indentation that draws its nesting,
and the ARIA that states it.

The list is flat — every node is a sibling of every other, which is what lets each
be its own lazy, keyed element — so the nesting a reader sees is drawn: the node
sits inside as many indentation boxes as it is deep, and each box draws its own
edge. Those boxes are presentation and carry no ARIA.

What they draw is said once, on the outermost element: `role="listitem"`,
`aria-level` for how deep it is, and `aria-labelledby` pointing at the title that
names it. A screen reader then hears "list item, level 3, Dataset name" where a
sighted reader sees the third indentation.

-}

import Common.Components.Questionnaire.Nodes exposing (NestingType(..))
import Common.Components.Questionnaire.Path as Path
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (attribute, class)


listItem : { level : Int, labelledBy : String, attributes : List (Attribute msg) } -> NestingType -> List (Html msg) -> Html msg
listItem config nestingType content =
    let
        -- The outermost indentation box is the list item itself: one element
        -- fewer for every node of a chapter than a wrapper around it would be.
        itemAttributes extraClass =
            class ("c-questionnaire-node c-questionnaire-nest" ++ extraClass)
                :: attribute "role" "listitem"
                :: attribute "aria-level" (String.fromInt config.level)
                :: attribute "aria-labelledby" config.labelledBy
                :: config.attributes
    in
    case nestingType of
        ContentNesting ->
            div (itemAttributes "") content

        FollowUpNesting inner ->
            div (itemAttributes " c-questionnaire-nest--follow-up") [ nest inner content ]

        ItemNesting inner ->
            div (itemAttributes " c-questionnaire-nest--item") [ nest inner content ]


nest : NestingType -> List (Html msg) -> Html msg
nest nestingType content =
    case nestingType of
        ContentNesting ->
            div [ class "c-questionnaire-nest" ] content

        FollowUpNesting inner ->
            div [ class "c-questionnaire-nest c-questionnaire-nest--follow-up" ] [ nest inner content ]

        ItemNesting inner ->
            div [ class "c-questionnaire-nest c-questionnaire-nest--item" ] [ nest inner content ]


{-| The id of the element that names the node at `path` — a question's title, an
item's heading.
-}
titleId : String -> String
titleId path =
    Path.toFieldId path ++ "-title"


{-| The id of a question's comments button, which focus goes back to when the
comments panel it opened is closed.
-}
commentsButtonId : String -> String
commentsButtonId path =
    Path.toFieldId path ++ "-comments"
