module Auth.Role exposing (isAdmin)

import Users.Common.Models exposing (User)


isAdmin : Maybe User -> Bool
isAdmin =
    Maybe.map (.role >> (==) "ADMIN") >> Maybe.withDefault False
