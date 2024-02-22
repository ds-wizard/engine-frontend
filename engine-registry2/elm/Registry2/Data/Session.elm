module Registry2.Data.Session exposing
    ( Session
    , decoder
    , encode
    , fromOrganization
    , init
    , setToken
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias Session =
    { organizationId : String
    , organizationName : String
    , token : String
    }


init : Session
init =
    { organizationId = ""
    , organizationName = ""
    , token = ""
    }


setToken : String -> Session -> Session
setToken token session =
    { session | token = token }


fromOrganization : { a | organizationId : String, name : String, token : String } -> Session
fromOrganization organization =
    { organizationId = organization.organizationId
    , organizationName = organization.name
    , token = organization.token
    }


decoder : Decoder Session
decoder =
    D.succeed Session
        |> D.required "organizationId" D.string
        |> D.required "organizationName" D.string
        |> D.required "token" D.string


encode : Session -> E.Value
encode session =
    E.object
        [ ( "organizationId", E.string session.organizationId )
        , ( "organizationName", E.string session.organizationName )
        , ( "token", E.string session.token )
        ]
