module Common.Data.TourId exposing
    ( dashboard
    , projectsCreate
    , projectsIndex
    , usersEditTours
    )

{-| The ids the server records a finished tour under.

Plain strings, because that is what `Common.Components.Tour` takes and what the
bootstrap configuration lists.

-}


dashboard : String
dashboard =
    "dashboard"


projectsCreate : String
projectsCreate =
    "projects_create"


projectsIndex : String
projectsIndex =
    "projects_index"


usersEditTours : String
usersEditTours =
    "users_edit_tours"
