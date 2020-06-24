module Shared.Data.Questionnaire.QuestionnaireLabel exposing
    ( QuestionnaireLabel
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias QuestionnaireLabel =
    { path : String
    , value : List String
    }


decoder : Decoder QuestionnaireLabel
decoder =
    D.succeed QuestionnaireLabel
        |> D.required "path" D.string
        |> D.required "value" (D.list D.string)


encode : QuestionnaireLabel -> E.Value
encode questionnaireLabel =
    E.object
        [ ( "path", E.string questionnaireLabel.path )
        , ( "value", E.list E.string questionnaireLabel.value )
        ]
