module Public.Routing exposing
    ( Route(..)
    , forgottenPasswordConfirmation
    , parsers
    , signupConfirmation
    , toUrl
    )

import Common.Config exposing (Config)
import Url.Parser exposing (..)


type Route
    = BookReference String
    | ForgottenPassword
    | ForgottenPasswordConfirmation String String
    | Login
    | Questionnaire
    | Signup
    | SignupConfirmation String String


parsers : Config -> (Route -> a) -> List (Parser (a -> c) c)
parsers config wrapRoute =
    let
        publicQuestionnaireRoutes =
            if config.publicQuestionnaireEnabled then
                [ map (wrapRoute <| Questionnaire) (s "questionnaire") ]

            else
                []

        signUpRoutes =
            if config.registrationEnabled then
                [ map (wrapRoute <| Signup) (s "signup")
                , map (signupConfirmation wrapRoute) (s "signup-confirmation" </> string </> string)
                ]

            else
                []
    in
    [ map (wrapRoute << BookReference) (s "book-references" </> string)
    , map (wrapRoute <| ForgottenPassword) (s "forgotten-password")
    , map (forgottenPasswordConfirmation wrapRoute) (s "forgotten-password" </> string </> string)
    , map (wrapRoute <| Login) top
    ]
        ++ publicQuestionnaireRoutes
        ++ signUpRoutes


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
