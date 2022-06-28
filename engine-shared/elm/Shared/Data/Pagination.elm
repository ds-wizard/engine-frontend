module Shared.Data.Pagination exposing (Pagination, decoder, empty)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Pagination.Page as Page exposing (Page)


type alias Pagination a =
    { items : List a
    , page : Page
    }


empty : Pagination a
empty =
    { items = []
    , page =
        { size = 0
        , totalElements = 0
        , totalPages = 0
        , number = 0
        }
    }


decoder : String -> Decoder a -> Decoder (Pagination a)
decoder itemsField itemDecoder =
    D.succeed Pagination
        |> D.requiredAt [ "_embedded", itemsField ] (D.list itemDecoder)
        |> D.required "page" Page.decoder
