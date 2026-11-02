module Registry.Api.Models.User exposing
    ( ApiKey
    , User
    , apiKeyDecoder
    , decoder
    , fullName
    , isAdmin
    )

{-| An account of the registry: an email, a name, a role. An
administrator publishes; everybody else reads and downloads, with the API keys
they make for their wizards.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias User =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , role : String
    , active : Bool
    , createdAt : Time.Posix
    }


decoder : Decoder User
decoder =
    D.succeed User
        |> D.required "uuid" Uuid.decoder
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "role" D.string
        |> D.required "active" D.bool
        |> D.required "createdAt" D.datetime


fullName : User -> String
fullName user =
    String.trim (user.firstName ++ " " ++ user.lastName)


isAdmin : User -> Bool
isAdmin user =
    user.role == "AdminRole"


{-| A key a wizard reads the registry with. Its value is shown once, when it is
made, and never again.
-}
type alias ApiKey =
    { uuid : Uuid
    , name : String
    , createdAt : Time.Posix
    }


apiKeyDecoder : Decoder ApiKey
apiKeyDecoder =
    D.succeed ApiKey
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "createdAt" D.datetime
