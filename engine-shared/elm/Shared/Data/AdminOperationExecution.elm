module Shared.Data.AdminOperationExecution exposing
    ( AdminOperationExecution
    , encode
    )

import Json.Encode as E


type alias AdminOperationExecution =
    { sectionName : String
    , operationName : String
    , parameters : List String
    }


encode : AdminOperationExecution -> E.Value
encode execution =
    E.object
        [ ( "sectionName", E.string execution.sectionName )
        , ( "operationName", E.string execution.operationName )
        , ( "parameters", E.list E.string execution.parameters )
        ]
