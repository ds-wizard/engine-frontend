module Shared.Elemental.Components.FileIconList exposing (..)

import Css exposing (hex)
import Css.Global exposing (descendants, typeSelector)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Shared.Elemental.Foundations.Icon as Icon
import Shared.Elemental.Foundations.Spacing as Spacing


view : List ( String, String ) -> Html msg
view formats =
    let
        styles =
            [ descendants
                [ typeSelector "svg"
                    [ Spacing.inlineMD
                    , Spacing.stackMD
                    ]
                ]
            ]

        toIcon ( shortName, color ) =
            Icon.file shortName (hex color)
    in
    div [ css styles ] (List.map toIcon formats)
