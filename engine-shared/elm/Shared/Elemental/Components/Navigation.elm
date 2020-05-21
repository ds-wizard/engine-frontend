module Shared.Elemental.Components.Navigation exposing
    ( ViewConfig
    , view
    )

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Shared.Elemental.Foundations.Shadow as Shadow
import Shared.Elemental.Foundations.Size as Size
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)


type alias ViewConfig =
    { appTitle : String
    , theme : Theme
    }


view : ViewConfig -> Html msg
view cfg =
    let
        styles =
            [ Shadow.xl Shadow.colorDefault cfg.theme
            , borderBottom3 (px 1) solid cfg.theme.colors.border
            , alignItems center
            , backgroundColor cfg.theme.colors.background
            , displayFlex
            , height (px2rem Size.navigationHeight)
            , left zero
            , padding2 zero (px2rem Spacing.gridComfortable)
            , position fixed
            , right zero
            , top zero
            , zIndex (int 10)
            , property "overscroll-behavior" "contain"
            ]
    in
    header [ css styles ]
        [ navbarBrand cfg
        ]


navbarBrand : ViewConfig -> Html msg
navbarBrand cfg =
    let
        styles =
            [ Typography.navbarBrand cfg.theme
            , backgroundImage (url cfg.theme.logo.url)
            , backgroundRepeat noRepeat
            , backgroundSize2 (px2rem cfg.theme.logo.width) (px2rem cfg.theme.logo.height)
            , minHeight (px2rem cfg.theme.logo.height)
            , displayFlex
            , alignItems center
            , paddingLeft (px2rem (cfg.theme.logo.width + 10))
            , textDecoration none
            ]
    in
    a [ css styles, href "/" ] [ text cfg.appTitle ]
