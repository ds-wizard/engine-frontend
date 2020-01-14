module Wizard.Common.JwtToken exposing (JwtToken, decoder, parse)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Jwt


type alias JwtToken =
    { permissions : List String }


parse : String -> Maybe JwtToken
parse token =
    case Jwt.decodeToken decoder token of
        Ok jwt ->
            Just jwt

        Err _ ->
            Nothing


decoder : Decoder JwtToken
decoder =
    D.succeed JwtToken
        |> D.required "permissions" (D.list D.string)
