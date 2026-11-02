module Common.Data.Context exposing
    ( WithGuideLinks
    , WithLocale
    , WithNavigator
    , WithTime
    )

{-| What a composite may know about the application it runs in.

Composites never take an application's `AppState`: each app defines its own,
and a component that named one would belong to that app. They take the few
fields they read instead, as extensible records, so any application whose state
has those fields can pass it straight in. The aliases are here so the same four
shapes are not spelled out slightly differently in every module.

-}

import Common.Data.Navigator exposing (Navigator)
import Common.Utils.GuideLinks exposing (GuideLinks)
import Gettext
import Time


{-| Everything user-visible is translated, so almost every composite needs this.
-}
type alias WithLocale a =
    { a | locale : Gettext.Locale }


{-| For anything that shows a time: relative ("3 days ago") needs the current
time, absolute needs the zone.
-}
type alias WithTime a =
    { a
        | locale : Gettext.Locale
        , timeZone : Time.Zone
        , currentTime : Time.Posix
    }


{-| For forms: which modifier submits depends on the platform.
-}
type alias WithNavigator a =
    { a
        | locale : Gettext.Locale
        , navigator : Navigator
    }


type alias WithGuideLinks a =
    { a
        | locale : Gettext.Locale
        , guideLinks : GuideLinks
    }
