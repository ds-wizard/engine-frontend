module Shared.Auth.Role exposing
    ( admin
    , dataSteward
    , options
    , researcher
    , toReadableString
    )

import Shared.Locale exposing (lg)
import Shared.Provisioning exposing (Provisioning)


admin : String
admin =
    "admin"


dataSteward : String
dataSteward =
    "dataSteward"


researcher : String
researcher =
    "researcher"


adminLocale : { a | provisioning : Provisioning } -> String
adminLocale =
    lg "role.admin"


dataStewardLocale : { a | provisioning : Provisioning } -> String
dataStewardLocale =
    lg "role.dataSteward"


researcherLocale : { a | provisioning : Provisioning } -> String
researcherLocale =
    lg "role.researcher"


options : { a | provisioning : Provisioning } -> List ( String, String )
options appState =
    [ ( researcher, researcherLocale appState )
    , ( dataSteward, dataStewardLocale appState )
    , ( admin, adminLocale appState )
    ]


toReadableString : { a | provisioning : Provisioning } -> String -> String
toReadableString appState role =
    if role == admin then
        adminLocale appState

    else if role == dataSteward then
        dataStewardLocale appState

    else
        researcherLocale appState
