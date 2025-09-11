module Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent.SetLabelsData exposing
    ( SetLabelsData
    , decoder
    , encode
    )

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time
import Uuid exposing (Uuid)


type alias SetLabelsData =
    { uuid : Uuid
    , path : String
    , value : List String
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : SetLabelsData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "SetLabelsEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "value", E.list E.string data.value )
        ]


decoder : Decoder SetLabelsData
decoder =
    D.succeed SetLabelsData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "value" (D.list D.string)
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
