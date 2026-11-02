module Common.Api.Models.Pagination exposing
    ( Pagination
    , decoder
    , empty
    )

import Common.Api.Models.Pagination.Page as Page exposing (Page)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Pagination a =
    { items : List a
    , page : Page
    }


empty : Pagination a
empty =
    { items = []
    , page = Page.empty
    }


decoder : String -> Decoder a -> Decoder (Pagination a)
decoder itemsField itemDecoder =
    D.succeed Pagination
        |> D.requiredAt [ "_embedded", itemsField ] (D.list itemDecoder)
        |> D.required "page" Page.decoder
