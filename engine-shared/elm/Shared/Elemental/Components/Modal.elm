module Shared.Elemental.Components.Modal exposing (..)

import Css exposing (..)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Shared.Elemental.Atoms.Button as Button
import Shared.Elemental.Foundations.Animation as Animation
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Shadow as Shadow
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)
import Shared.Html.Styled exposing (emptyNode)


type alias ModalConfig msg =
    { visible : Bool
    , closeMsg : msg
    }


view : ModalConfig msg -> Theme -> List (Html msg) -> Html msg
view cfg theme content =
    let
        wrapperStyles =
            [ position fixed
            , top zero
            , left zero
            , right zero
            , bottom zero
            , displayFlex
            , alignItems center
            , justifyContent center
            , zIndex (int 1000)
            ]

        overlayStyles =
            [ position absolute
            , top zero
            , left zero
            , right zero
            , bottom zero
            , backgroundColor theme.colors.overlay
            , zIndex (int 1000)
            ]

        modalStyles =
            [ Border.roundedDefault
            , Shadow.xl Shadow.colorDefault theme
            , backgroundColor theme.colors.background
            , maxWidth (pct 100)
            , maxHeight (pct 100)
            , margin (px2rem Spacing.md)
            , zIndex (int 1100)
            ]

        closeButtonStyles =
            [ position absolute
            , right zero
            , top zero
            , important Spacing.insetSquishSM
            ]

        contentStyles =
            [ maxHeight (calc (vh 100) minus (px2rem (2 * Spacing.lg)))
            , marginTop (px2rem Spacing.lg)
            , marginBottom (px2rem Spacing.md)
            , overflowY scroll
            ]
    in
    if cfg.visible then
        div [ css wrapperStyles, Animation.fadeIn, Animation.fast ]
            [ div [ css overlayStyles, onClick cfg.closeMsg ] []
            , div [ css modalStyles, Animation.moveUp, Animation.fast ]
                [ Button.link theme
                    [ onClick cfg.closeMsg
                    , css closeButtonStyles
                    ]
                    [ text "×" ]
                , div [ css contentStyles ] content
                ]
            ]

    else
        emptyNode
