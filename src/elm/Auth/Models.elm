module Auth.Models exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias Model =
    { email : String
    , password : String
    , error : String
    , loading : Bool
    }


type alias Session =
    { token : String
    , user : Maybe User
    }


type alias User =
    { uuid : String
    , email : String
    , name : String
    , surname : String
    }



-- Auth Model helpers


initialModel : Model
initialModel =
    { email = ""
    , password = ""
    , error = ""
    , loading = False
    }


updateEmail : Model -> String -> Model
updateEmail model email =
    { model | email = email }


updatePassword : Model -> String -> Model
updatePassword model password =
    { model | password = password }


updateError : Model -> String -> Model
updateError model error =
    { model | error = error }


updateLoading : Model -> Bool -> Model
updateLoading model loading =
    { model | loading = loading }



-- Session helpers


initialSession : Session
initialSession =
    { token = ""
    , user = Nothing
    }


setToken : Session -> String -> Session
setToken session token =
    { session | token = token }


setUser : Session -> User -> Session
setUser session user =
    { session | user = Just user }


decodeSession : Decoder Session
decodeSession =
    decode Session
        |> required "token" Decode.string
        |> required "user" (Decode.nullable userDecoder)


sessionExists : Session -> Bool
sessionExists session =
    session.token /= ""



-- User helpers


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "email" Decode.string
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "surname" Decode.string
