module Auth.Models exposing (..)

{-| Auth module model and helpers.


# Types

@docs Model, Session, JwtToken


# Auth Model helpers

@docs initialModel, updateEmail, updatePassword, updateError, updateLoading


# Session helpers

@docs initialSession, setToken, setUser, sessionDecoder, sessionExists


# JWT helpers

@docs parseJwt, jwtDecoder

-}

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Jwt exposing (decodeToken)
import UserManagement.Models exposing (User, userDecoder)


{-| -}
type alias Model =
    { email : String
    , password : String
    , error : String
    , loading : Bool
    }


{-| -}
type alias Session =
    { token : String
    , user : Maybe User
    }


{-| -}
type alias JwtToken =
    { permissions : List String }



-- Auth Model helpers


{-| -}
initialModel : Model
initialModel =
    { email = ""
    , password = ""
    , error = ""
    , loading = False
    }


{-| -}
updateEmail : Model -> String -> Model
updateEmail model email =
    { model | email = email }


{-| -}
updatePassword : Model -> String -> Model
updatePassword model password =
    { model | password = password }


{-| -}
updateError : Model -> String -> Model
updateError model error =
    { model | error = error }


{-| -}
updateLoading : Model -> Bool -> Model
updateLoading model loading =
    { model | loading = loading }



-- Session helpers


{-| -}
initialSession : Session
initialSession =
    { token = ""
    , user = Nothing
    }


{-| -}
setToken : Session -> String -> Session
setToken session token =
    { session | token = token }


{-| -}
setUser : Session -> User -> Session
setUser session user =
    { session | user = Just user }


{-| -}
sessionDecoder : Decoder Session
sessionDecoder =
    decode Session
        |> required "token" Decode.string
        |> required "user" (Decode.nullable userDecoder)


{-| -}
sessionExists : Session -> Bool
sessionExists session =
    session.token /= ""



-- JWT helpers


{-| -}
parseJwt : String -> Maybe JwtToken
parseJwt token =
    case decodeToken jwtDecoder token of
        Ok jwt ->
            Just jwt

        Err error ->
            Nothing


{-| -}
jwtDecoder : Decoder JwtToken
jwtDecoder =
    decode JwtToken
        |> required "permissions" (Decode.list Decode.string)
