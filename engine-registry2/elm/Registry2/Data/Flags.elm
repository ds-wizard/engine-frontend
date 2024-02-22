module Registry2.Data.Flags exposing
    ( Flags
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry2.Data.Session as Session exposing (Session)


type alias Flags =
    { apiUrl : String
    , session : Maybe Session
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "apiUrl" D.string
        |> D.required "session" (D.maybe Session.decoder)


default : Flags
default =
    { apiUrl = ""
    , session = Nothing
    }
