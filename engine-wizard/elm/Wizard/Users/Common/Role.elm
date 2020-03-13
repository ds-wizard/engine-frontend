module Wizard.Users.Common.Role exposing
    ( admin
    , dataSteward
    , list
    , researcher
    )


admin : String
admin =
    "ADMIN"


dataSteward : String
dataSteward =
    "DATASTEWARD"


researcher : String
researcher =
    "RESEARCHER"


list : List String
list =
    [ admin, dataSteward, researcher ]
