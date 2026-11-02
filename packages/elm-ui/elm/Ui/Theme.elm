port module Ui.Theme exposing
    ( Mode(..)
    , Theme
    , default
    , modeToAttribute
    , setMode
    , setTheme
    , sheetFor
    , toStyleString
    , withSeed
    )

{-| The runtime theme: a seed color resolved into brand roles for both modes,
emitted as a style block under two scopes.

The neutral ramps are static and live in `_roles.scss`; only the brand roles are
derived, because only they depend on tenant configuration.

-}

import Color exposing (Color)
import Ui.Theme.Check as Check
import Ui.Theme.Derive as Derive
import Ui.Theme.Palette as Palette


type alias Theme =
    { seed : Color }


type Mode
    = Light
    | Dark
    | System


default : Theme
default =
    { seed = Derive.defaultSeed }


withSeed : Color -> Theme -> Theme
withSeed seed theme =
    { theme | seed = seed }


modeToAttribute : Mode -> Maybe String
modeToAttribute mode =
    case mode of
        Light ->
            Just "light"

        Dark ->
            Just "dark"

        System ->
            Nothing


{-| Injects the derived brand roles as a style element. `toStyleString` emits
selector blocks for both modes, so this cannot go in a style attribute.
-}
setTheme : Theme -> Cmd msg
setTheme =
    uiSetThemePort << toStyleString


{-| Stamps `data-theme` on the document element. Every role is scoped to
`:root`, so the attribute has to be set there and nowhere else — which Elm
cannot do through the virtual DOM.
-}
setMode : Mode -> Cmd msg
setMode mode =
    uiSetModePort (Maybe.withDefault "system" (modeToAttribute mode))


port uiSetThemePort : String -> Cmd msg


port uiSetModePort : String -> Cmd msg


{-| Emits both scopes. Components reference roles only, so a component
stylesheet is written once and never mentions a mode.
-}
toStyleString : Theme -> String
toStyleString theme =
    let
        light =
            Derive.lightRoles Palette.lightConstraints theme.seed

        dark =
            Derive.darkRoles Palette.darkConstraints theme.seed

        block roles =
            String.concat
                [ property "--ui-brand" roles.base
                , property "--ui-brand-strong" roles.strong
                , property "--ui-brand-hover" roles.hover
                , property "--ui-brand-active" roles.active
                , property "--ui-brand-subtle" roles.subtle
                , property "--ui-brand-subtle-hover" roles.subtleHover
                , property "--ui-brand-subtle-selected" roles.subtleSelected
                , property "--ui-brand-on-subtle" roles.onSubtle
                , property "--ui-link" roles.link
                , property "--ui-link-hover" roles.linkHover
                ]
    in
    String.concat
        [ ":root,:root[data-theme=\"light\"]{"
        , block light
        , "}"
        , "@media (prefers-color-scheme: dark){:root:not([data-theme=\"light\"]){"
        , block dark
        , "}}"
        , ":root[data-theme=\"dark\"]{"
        , block dark
        , "}"
        ]


{-| The resolved sheet for a mode, for `Check.violations` and the styleguide's
live readout.
-}
sheetFor : Mode -> Theme -> Check.Sheet
sheetFor mode theme =
    case mode of
        Dark ->
            Palette.darkSheet (Derive.darkRoles Palette.darkConstraints theme.seed)

        _ ->
            Palette.lightSheet (Derive.lightRoles Palette.lightConstraints theme.seed)


property : String -> Color -> String
property name color =
    name ++ ":" ++ Palette.toHex color ++ ";"
