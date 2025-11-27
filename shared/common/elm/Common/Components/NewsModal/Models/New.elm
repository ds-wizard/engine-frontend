module Common.Components.NewsModal.Models.New exposing
    ( New
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias New =
    { id : String
    , title : String
    , image : String
    , content : String
    }


decoder : Decoder New
decoder =
    D.succeed New
        |> D.required "id" D.string
        |> D.required "title" D.string
        |> D.required "image" D.string
        |> D.required "content" D.string
