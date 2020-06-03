module Shared.Elemental.Atoms.Text exposing (danger, lighter, success)

import Css exposing (Style)
import Html.Styled exposing (Html, div, fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Markdown
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)


danger : Theme -> String -> Html msg
danger theme =
    text [ Typography.copy1danger theme ]


success : Theme -> String -> Html msg
success theme =
    text [ Typography.copy1success theme ]


lighter : Theme -> String -> Html msg
lighter theme =
    text [ Typography.copy1lighter theme ]


text : List Style -> String -> Html msg
text styles value =
    div [ css styles ] [ fromUnstyled <| Markdown.toHtml [] value ]
