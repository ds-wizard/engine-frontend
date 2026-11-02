module Common.Api.Models.WorkspaceMember exposing
    ( MemberRole
    , WorkspaceMember
    , decoder
    )

{-| A user who is a member of a workspace. Membership is the floor of what a
user sees in it. A member has a role in the workspace, which
adds to what their organization role allows. The shape is the server's
`WorkspaceMember`: the picture is the uploaded image or the Gravatar of
`gravatarHash`, as for every user; there is no affiliation.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias WorkspaceMember =
    { uuid : Uuid
    , firstName : String
    , lastName : String
    , email : String
    , imageUrl : Maybe String
    , gravatarHash : String
    , createdAt : Time.Posix
    , role : MemberRole
    }


type alias MemberRole =
    { uuid : Uuid
    , name : String
    }


decoder : Decoder WorkspaceMember
decoder =
    D.succeed WorkspaceMember
        |> D.required "uuid" Uuid.decoder
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "email" D.string
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.required "gravatarHash" D.string
        |> D.required "createdAt" D.datetime
        |> D.required "role" roleDecoder


roleDecoder : Decoder MemberRole
roleDecoder =
    D.succeed MemberRole
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
