module Common.Api.Models.Role exposing (Role, decoder, localizedName, toFormOptions)

import Common.Api.Models.RolePermission as RolePermission exposing (RolePermission)
import Gettext exposing (Locale, gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias Role =
    { uuid : Uuid
    , isAdmin : Bool
    , name : String
    , permissions : List RolePermission
    , usersCount : Int
    }


decoder : Decoder Role
decoder =
    D.succeed Role
        |> D.required "uuid" Uuid.decoder
        |> D.required "isAdmin" D.bool
        |> D.required "name" D.string
        |> D.required "permissions" (D.list RolePermission.decoder)
        |> D.required "usersCount" D.int


{-| Roles can be defined by users, so their names are not known at compile time and cannot be
extracted to the POT file. Passing the name through gettext with a variable (which the POT
extraction script ignores, unlike `gettext "string"`) still makes it translatable -- the string can
be added to the PO files manually when importing locales.

It works for any role-like record with a name, such as `Role` or `RoleInfo`.

-}
localizedName : Locale -> { a | name : String } -> String
localizedName locale role =
    gettext role.name locale


{-| Form options for role selects, sorted alphabetically by the localized role name.
-}
toFormOptions : Locale -> List Role -> List ( String, String )
toFormOptions locale roles =
    roles
        |> List.map (\role -> ( Uuid.toString role.uuid, localizedName locale role ))
        |> List.sortBy (String.toLower << Tuple.second)
