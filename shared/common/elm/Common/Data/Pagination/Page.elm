module Common.Data.Pagination.Page exposing
    ( Page
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Page =
    { size : Int
    , totalElements : Int
    , totalPages : Int
    , number : Int
    }


decoder : Decoder Page
decoder =
    D.succeed Page
        |> D.required "size" D.int
        |> D.required "totalElements" D.int
        |> D.required "totalPages" D.int
        |> D.required "number" D.int
