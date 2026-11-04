module Common.Api.Settings exposing
    ( Section
    , delete
    , get
    , put
    )

{-| The settings, one section at a time: `/settings/<section>`.

Whose value a request reads or writes is the application's to say
(`Common.Api.Params`), as for every other request.

-}

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Settings as Settings exposing (Settings)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Json.Decode exposing (Decoder)
import Json.Encode as E


{-| A section: its name in the address, and how its value is read and written.
-}
type alias Section value =
    { name : String
    , decoder : Decoder value
    , encode : value -> E.Value
    }


get : Context ctx -> Section value -> ToMsg (Settings value) msg -> Cmd msg
get appState section =
    Request.get (Context.toServerInfo appState)
        (Params.apply appState "GET" (url section))
        (Settings.decoder section.decoder)


put : Context ctx -> Section value -> Settings value -> ToMsg () msg -> Cmd msg
put appState section settings =
    Request.putWhatever (Context.toServerInfo appState)
        (Params.apply appState "PUT" (url section))
        (Settings.encode section.encode settings)


{-| Give up a value of one's own, back to the defaults.
-}
delete : Context ctx -> Section value -> ToMsg () msg -> Cmd msg
delete appState section =
    Request.delete (Context.toServerInfo appState)
        (Params.apply appState "DELETE" (url section))


url : Section value -> String
url section =
    "/settings/" ++ section.name
