module Wizard.Api.Models.KnowledgeModelMigrationResolution exposing
    ( KnowledgeModelMigrationResolution
    , apply
    , encode
    , reject
    )

import Json.Encode as E


type alias KnowledgeModelMigrationResolution =
    { originalEventUuid : String
    , action : String
    }


create : String -> String -> KnowledgeModelMigrationResolution
create action uuid =
    { originalEventUuid = uuid
    , action = action
    }


apply : String -> KnowledgeModelMigrationResolution
apply =
    create "ApplyKnowledgeModelMigrationAction"


reject : String -> KnowledgeModelMigrationResolution
reject =
    create "RejectKnowledgeModelMigrationAction"


encode : KnowledgeModelMigrationResolution -> E.Value
encode data =
    E.object
        [ ( "originalEventUuid", E.string data.originalEventUuid )
        , ( "action", E.string data.action )
        , ( "event", E.null )
        ]
