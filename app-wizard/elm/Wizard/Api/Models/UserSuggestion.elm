module Wizard.Api.Models.UserSuggestion exposing (UserSuggestion, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)


type alias UserSuggestion =
    { uuid : Uuid
    , firstName : String
    , lastName : String
    , gravatarHash : String
    , imageUrl : Maybe String
    }


decoder : Decoder UserSuggestion
decoder =
    D.succeed UserSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "gravatarHash" D.string
        |> D.required "imageUrl" (D.maybe D.string)


encode : UserSuggestion -> E.Value
encode userSuggestion =
    E.object
        [ ( "uuid", Uuid.encode userSuggestion.uuid )
        , ( "firstName", E.string userSuggestion.firstName )
        , ( "lastName", E.string userSuggestion.lastName )
        , ( "gravatarHash", E.string userSuggestion.gravatarHash )
        , ( "imageUrl", E.maybe E.string userSuggestion.imageUrl )
        ]
