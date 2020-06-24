module Shared.Data.QuestionnaireDetail.FormValue exposing
    ( FormValue
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.QuestionnaireDetail.FormValue.ReplyValue as ReplyValue exposing (ReplyValue)


type alias FormValue =
    { path : String
    , value : ReplyValue
    }


decoder : Decoder FormValue
decoder =
    D.succeed FormValue
        |> D.required "path" D.string
        |> D.required "value" ReplyValue.decoder


encode : FormValue -> E.Value
encode formValue =
    E.object
        [ ( "path", E.string formValue.path )
        , ( "value", ReplyValue.encode formValue.value )
        ]
