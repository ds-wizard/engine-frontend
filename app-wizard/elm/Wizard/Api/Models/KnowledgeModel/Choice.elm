module Wizard.Api.Models.KnowledgeModel.Choice exposing
    ( Choice
    , decoder
    , equalContent
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias Choice =
    { uuid : String
    , label : String
    , annotations : List Annotation
    }


decoder : Decoder Choice
decoder =
    D.succeed Choice
        |> D.required "uuid" D.string
        |> D.required "label" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


equalContent : Choice -> Choice -> Bool
equalContent choice1 choice2 =
    choice1.label == choice2.label
