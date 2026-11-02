module Dsw.Data.ClientDefaults exposing (clientDefaults)

{-| The client's own: the DSW registry, and the project's
repository for help, and its brand color (`scss/_brand.scss`).
-}

import Common.Data.ClientDefaults exposing (ClientDefaults)


clientDefaults : ClientDefaults
clientDefaults =
    { registryName = "DSW Registry"
    , registryUrl = "https://registry.ds-wizard.org"
    , supportEmail = "support@ds-wizard.org"
    , supportSiteName = "ds-wizard/ds-wizard"
    , supportSiteUrl = "https://github.com/ds-wizard/ds-wizard"
    , supportSiteIcon = "fab fa-github"
    , primaryColor = "#F15A24"
    , translationKind = "dsw-client"
    }
