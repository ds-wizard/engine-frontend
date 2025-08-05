module Wizard.Api.Models.QuestionnairePerm exposing
    ( admin
    , all
    , comment
    , edit
    , view
    )


view : String
view =
    "VIEW"


comment : String
comment =
    "COMMENT"


edit : String
edit =
    "EDIT"


admin : String
admin =
    "ADMIN"


all : List String
all =
    [ view, comment, edit, admin ]
