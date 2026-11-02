module Common.Api.Models.UserGroupDetail exposing
    ( UserGroupDetail
    , decoder
    )

import Common.Api.Models.UserGroupMember as UserGroupMember exposing (UserGroupMember)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


{-| One user group with its members. `workspaceUuid` is the workspace it is in
(user groups are workspace-only); a server that does not send it yet
leaves it `Nothing`.
-}
type alias UserGroupDetail =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , private : Bool
    , users : List UserGroupMember
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder UserGroupDetail
decoder =
    D.succeed UserGroupDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "private" D.bool
        |> D.required "users" (D.list UserGroupMember.decoder)
        |> D.required "workspaceUuid" (D.nullable Uuid.decoder)
