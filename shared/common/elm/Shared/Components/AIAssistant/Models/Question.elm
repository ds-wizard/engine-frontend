module Shared.Components.AIAssistant.Models.Question exposing
    ( Question
    , encode
    )

import Json.Encode as E


type alias Question =
    { question : String
    }


encode : Question -> E.Value
encode question =
    E.object
        [ ( "question", E.string question.question )
        ]
