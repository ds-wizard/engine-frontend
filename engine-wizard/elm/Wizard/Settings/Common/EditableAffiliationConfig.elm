module Wizard.Settings.Common.EditableAffiliationConfig exposing
    ( EditableAffiliationConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias EditableAffiliationConfig =
    { affiliations : List String
    }


decoder : Decoder EditableAffiliationConfig
decoder =
    D.succeed EditableAffiliationConfig
        |> D.required "affiliations" (D.list D.string)


encode : EditableAffiliationConfig -> E.Value
encode config =
    E.object
        [ ( "affiliations", E.list E.string config.affiliations )
        ]
