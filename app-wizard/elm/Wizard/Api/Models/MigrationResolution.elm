module Wizard.Api.Models.MigrationResolution exposing
    ( MigrationResolution
    , apply
    , encode
    , reject
    )

import Json.Encode as E


type alias MigrationResolution =
    { originalEventUuid : String
    , action : String
    }


create : String -> String -> MigrationResolution
create action uuid =
    { originalEventUuid = uuid
    , action = action
    }


apply : String -> MigrationResolution
apply =
    create "Apply"


reject : String -> MigrationResolution
reject =
    create "Reject"


encode : MigrationResolution -> E.Value
encode data =
    E.object
        [ ( "originalEventUuid", E.string data.originalEventUuid )
        , ( "action", E.string data.action )
        , ( "event", E.null )
        ]
