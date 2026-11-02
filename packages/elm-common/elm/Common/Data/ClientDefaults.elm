module Common.Data.ClientDefaults exposing
    ( ClientDefaults
    , none
    )

{-| What a client falls back on where the server says nothing: the name and the
address of the registry it imports from, where its users go for help, and the
color it is drawn in. Each
client fills it in with its own at start-up; the shared pages only read it.
-}


type alias ClientDefaults =
    { registryName : String
    , registryUrl : String
    , supportEmail : String
    , supportSiteName : String
    , supportSiteUrl : String
    , supportSiteIcon : String

    -- The brand color, as a hex: what the application is drawn in where
    -- neither the settings nor a workspace choose one.
    , primaryColor : String

    -- The kind of its translations' POT, such as `dsw-client`:
    -- the start of a translation's `Project-Id-Version`.
    , translationKind : String
    }


{-| Nothing of a client's own: a registry called "Registry" at no address, and
no support, and no color.
-}
none : ClientDefaults
none =
    { registryName = "Registry"
    , registryUrl = ""
    , supportEmail = ""
    , supportSiteName = ""
    , supportSiteUrl = ""
    , supportSiteIcon = ""
    , primaryColor = ""
    , translationKind = ""
    }
