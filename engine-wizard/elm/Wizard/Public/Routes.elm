module Wizard.Public.Routes exposing (Route(..))


type Route
    = AuthCallback String (Maybe String) (Maybe String) (Maybe String)
    | BookReferenceRoute String
    | ForgottenPasswordRoute
    | ForgottenPasswordConfirmationRoute String String
    | LoginRoute (Maybe String)
    | SignupRoute
    | SignupConfirmationRoute String String
