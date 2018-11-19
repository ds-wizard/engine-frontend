module Public.Routing exposing (Route(..), forgottenPasswordConfirmation, parsers, signupConfirmation, toUrl)

import Url.Parser exposing (..)


type Route
    = BookReference String
    | ForgottenPassword
    | ForgottenPasswordConfirmation String String
    | Login
    | Questionnaire
    | Signup
    | SignupConfirmation String String


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute << BookReference) (s "book-references" </> string)
    , map (wrapRoute <| ForgottenPassword) (s "forgotten-password")
    , map (forgottenPasswordConfirmation wrapRoute) (s "forgotten-password" </> string </> string)
    , map (wrapRoute <| Login) top
    , map (wrapRoute <| Questionnaire) (s "questionnaire")
    , map (wrapRoute <| Signup) (s "signup")
    , map (signupConfirmation wrapRoute) (s "signup-confirmation" </> string </> string)
    ]


signupConfirmation : (Route -> a) -> String -> String -> a
signupConfirmation wrapRoute userId hash =
    SignupConfirmation userId hash |> wrapRoute


forgottenPasswordConfirmation : (Route -> a) -> String -> String -> a
forgottenPasswordConfirmation wrapRoute userId hash =
    ForgottenPasswordConfirmation userId hash |> wrapRoute


toUrl : Route -> List String
toUrl route =
    case route of
        BookReference uuid ->
            [ "book-references", uuid ]

        ForgottenPassword ->
            [ "forgotten-password" ]

        ForgottenPasswordConfirmation userId hash ->
            [ "forgotten-password", userId, hash ]

        Login ->
            []

        Questionnaire ->
            [ "questionnaire" ]

        Signup ->
            [ "signup" ]

        SignupConfirmation userId hash ->
            [ "signup-confirmation", userId, hash ]
