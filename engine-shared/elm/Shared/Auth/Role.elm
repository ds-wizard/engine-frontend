module Shared.Auth.Role exposing
    ( admin
    , dataSteward
    , options
    , researcher
    , switch
    , toReadableString
    )

import Gettext exposing (gettext)


admin : String
admin =
    "admin"


dataSteward : String
dataSteward =
    "dataSteward"


researcher : String
researcher =
    "researcher"


adminLocale : Gettext.Locale -> String
adminLocale =
    gettext "Admin"


dataStewardLocale : Gettext.Locale -> String
dataStewardLocale =
    gettext "Data Steward"


researcherLocale : Gettext.Locale -> String
researcherLocale =
    gettext "Researcher"


options : { a | locale : Gettext.Locale } -> List ( String, String )
options appState =
    [ ( researcher, researcherLocale appState.locale )
    , ( dataSteward, dataStewardLocale appState.locale )
    , ( admin, adminLocale appState.locale )
    ]


toReadableString : { a | locale : Gettext.Locale } -> String -> String
toReadableString appState role =
    if role == admin then
        adminLocale appState.locale

    else if role == dataSteward then
        dataStewardLocale appState.locale

    else
        researcherLocale appState.locale


switch : String -> a -> a -> a -> a -> a
switch role adminValue dataStewardValue researcherValue defaultValue =
    if role == admin then
        adminValue

    else if role == dataSteward then
        dataStewardValue

    else if role == researcher then
        researcherValue

    else
        defaultValue
