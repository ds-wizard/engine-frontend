module Common.Data.News exposing
    ( News
    , decoder
    )

{-| One item of "What's new": a title, a picture and a markdown body.

Data only, so it lives outside `Components`: the request that fetches it and
the dialog that shows it both need the type, and neither should have to import
the other to get it.

-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias News =
    { id : String
    , title : String
    , image : String
    , content : String
    }


decoder : Decoder News
decoder =
    D.succeed News
        |> D.required "id" D.string
        |> D.required "title" D.string
        |> D.required "image" D.string
        |> D.required "content" D.string
