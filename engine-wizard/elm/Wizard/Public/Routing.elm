module Wizard.Public.Routing exposing
    ( forgottenPasswordConfirmation
    , parsers
    , signupConfirmation
    , toUrl
    )

import Shared.Locale exposing (lr)
import Url exposing (percentEncode)
import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.Public.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        publicQuestionnaireRoutes =
            if appState.config.publicQuestionnaireEnabled then
                [ map (wrapRoute <| QuestionnaireRoute) (s (lr "public.questionnaire" appState)) ]

            else
                []

        signUpRoutes =
            if appState.config.registrationEnabled then
                [ map (wrapRoute <| SignupRoute) (s (lr "public.signup" appState))
                , map (signupConfirmation wrapRoute) (s (lr "public.signup" appState) </> string </> string)
                ]

            else
                []
    in
    [ map (wrapRoute << BookReferenceRoute) (s (lr "public.bookReferences" appState) </> string)
    , map (wrapRoute <| ForgottenPasswordRoute) (s (lr "public.forgottenPassword" appState))
    , map (forgottenPasswordConfirmation wrapRoute) (s (lr "public.forgottenPassword" appState) </> string </> string)
    , map (wrapRoute << LoginRoute) (top <?> Query.string (lr "login.originalUrl" appState))
    ]
        ++ publicQuestionnaireRoutes
        ++ signUpRoutes


signupConfirmation : (Route -> a) -> String -> String -> a
signupConfirmation wrapRoute userId hash =
    SignupConfirmationRoute userId hash |> wrapRoute


forgottenPasswordConfirmation : (Route -> a) -> String -> String -> a
forgottenPasswordConfirmation wrapRoute userId hash =
    ForgottenPasswordConfirmationRoute userId hash |> wrapRoute


toUrl : AppState -> Route -> List String
toUrl appState route =
    case route of
        BookReferenceRoute uuid ->
            [ lr "public.bookReferences" appState, uuid ]

        ForgottenPasswordRoute ->
            [ lr "public.forgottenPassword" appState ]

        ForgottenPasswordConfirmationRoute userId hash ->
            [ lr "public.forgottenPassword" appState, userId, hash ]

        LoginRoute mbOriginalUrl ->
            case mbOriginalUrl of
                Just originalUrl ->
                    [ "/?" ++ lr "login.originalUrl" appState ++ "=" ++ percentEncode originalUrl ]

                Nothing ->
                    []

        QuestionnaireRoute ->
            [ lr "public.questionnaire" appState ]

        SignupRoute ->
            [ lr "public.signup" appState ]

        SignupConfirmationRoute userId hash ->
            [ lr "public.signup" appState, userId, hash ]
