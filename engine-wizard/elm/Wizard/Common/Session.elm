module Wizard.Common.Session exposing
    ( Session
    , decoder
    , exists
    , init
    , setSidebarCollapsed
    , setToken
    , setUser
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Users.Common.User as User exposing (User)


type alias Session =
    { token : String
    , user : Maybe User
    , sidebarCollapsed : Bool
    , v1 : Bool
    }


init : Session
init =
    { token = ""
    , user = Nothing
    , sidebarCollapsed = False
    , v1 = True
    }


setToken : Session -> String -> Session
setToken session token =
    { session | token = token }


setUser : Session -> User -> Session
setUser session user =
    { session | user = Just user }


setSidebarCollapsed : Session -> Bool -> Session
setSidebarCollapsed session collapsed =
    { session | sidebarCollapsed = collapsed }


decoder : Decoder Session
decoder =
    D.succeed Session
        |> D.required "token" D.string
        |> D.required "user" (D.nullable User.decoder)
        |> D.optional "sidebarCollapsed" D.bool False
        |> D.required "v1" D.bool


exists : Session -> Bool
exists session =
    session.token /= ""
