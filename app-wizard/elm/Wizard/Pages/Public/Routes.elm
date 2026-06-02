module Wizard.Pages.Public.Routes exposing (Route(..))


type Route
    = ForgottenPasswordRoute
    | ForgottenPasswordConfirmationRoute String String
    | LoginRoute (Maybe String)
    | LogoutSuccessful
    | OpenIdCallback String (Maybe String) (Maybe String) (Maybe String)
    | SignupRoute
    | SignupConfirmationRoute String String
