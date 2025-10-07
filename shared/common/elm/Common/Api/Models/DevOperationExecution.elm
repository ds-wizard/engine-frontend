module Common.Api.Models.DevOperationExecution exposing
    ( DevOperationExecution
    , encode
    )

import Json.Encode as E


type alias DevOperationExecution =
    { sectionName : String
    , operationName : String
    , parameters : List String
    }


encode : DevOperationExecution -> E.Value
encode execution =
    E.object
        [ ( "sectionName", E.string execution.sectionName )
        , ( "operationName", E.string execution.operationName )
        , ( "parameters", E.list E.string execution.parameters )
        ]
