module Wizard.Common.Pagination.Pagination exposing (Pagination, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Pagination.Page as Page exposing (Page)


type alias Pagination a =
    { items : List a
    , page : Page
    }


decoder : String -> Decoder a -> Decoder (Pagination a)
decoder itemsField itemDecoder =
    D.succeed Pagination
        |> D.requiredAt [ "_embedded", itemsField ] (D.list itemDecoder)
        |> D.required "page" Page.decoder
