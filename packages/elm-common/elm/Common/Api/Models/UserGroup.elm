module Common.Api.Models.UserGroup exposing
    ( UserGroup
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


{-| A user group as the list shows it. `member` and `editable` are the reader's:
whether they are in it, and whether they may change it.
-}
type alias UserGroup =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , member : Bool
    , private : Bool
    , editable : Bool
    , updatedAt : Time.Posix
    , workspaceUuid : Maybe Uuid
    }


decoder : Decoder UserGroup
decoder =
    D.succeed UserGroup
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "member" D.bool
        |> D.required "private" D.bool
        |> D.required "editable" D.bool
        |> D.required "updatedAt" D.datetime
        |> D.required "workspaceUuid" (D.nullable Uuid.decoder)
