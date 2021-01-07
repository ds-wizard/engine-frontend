module Shared.Data.QuestionnairePerm exposing
    ( admin
    , all
    , edit
    , view
    )


view : String
view =
    "VIEW"


edit : String
edit =
    "EDIT"


admin : String
admin =
    "ADMIN"


all : List String
all =
    [ view, edit, admin ]
