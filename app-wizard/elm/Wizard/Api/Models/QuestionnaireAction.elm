module Wizard.Api.Models.QuestionnaireAction exposing
    ( QuestionnaireAction
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias QuestionnaireAction =
    { id : String
    , name : String
    , description : String
    , url : String
    , enabled : Bool
    }


decoder : Decoder QuestionnaireAction
decoder =
    D.succeed QuestionnaireAction
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.optional "description" D.string "Default"
        |> D.required "url" D.string
        |> D.required "enabled" D.bool


encode : QuestionnaireAction -> E.Value
encode questionnaireAction =
    E.object
        [ ( "enabled", E.bool questionnaireAction.enabled ) ]
