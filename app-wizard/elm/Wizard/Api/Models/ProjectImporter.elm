module Wizard.Api.Models.ProjectImporter exposing
    ( ProjectImporter
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias ProjectImporter =
    { id : String
    , name : String
    , description : String
    , url : String
    , enabled : Bool
    }


decoder : Decoder ProjectImporter
decoder =
    D.succeed ProjectImporter
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.optional "description" D.string "Default"
        |> D.required "url" D.string
        |> D.required "enabled" D.bool


encode : ProjectImporter -> E.Value
encode questionnaireImporter =
    E.object
        [ ( "enabled", E.bool questionnaireImporter.enabled ) ]
