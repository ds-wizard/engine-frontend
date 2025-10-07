module Wizard.Pages.Public.Routes exposing (Route(..))


type Route
    = AuthCallback String (Maybe String) (Maybe String) (Maybe String)
    | ForgottenPasswordRoute
    | ForgottenPasswordConfirmationRoute String String
    | LoginRoute (Maybe String)
    | LogoutSuccessful
    | SignupRoute
    | SignupConfirmationRoute String String
