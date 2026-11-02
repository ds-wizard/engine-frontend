module Common.Api.Models.Pagination.Page exposing
    ( Page
    , decoder
    , empty
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Page =
    { size : Int
    , totalElements : Int
    , totalPages : Int
    , number : Int
    }


empty : Page
empty =
    { size = 0
    , totalElements = 0
    , totalPages = 0
    , number = 0
    }


decoder : Decoder Page
decoder =
    D.succeed Page
        |> D.required "size" D.int
        |> D.required "totalElements" D.int
        |> D.required "totalPages" D.int
        |> D.required "number" D.int
