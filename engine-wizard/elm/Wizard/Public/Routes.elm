module Wizard.Public.Routes exposing (Route(..))


type Route
    = BookReferenceRoute String
    | ForgottenPasswordRoute
    | ForgottenPasswordConfirmationRoute String String
    | LoginRoute
    | QuestionnaireRoute
    | SignupRoute
    | SignupConfirmationRoute String String
