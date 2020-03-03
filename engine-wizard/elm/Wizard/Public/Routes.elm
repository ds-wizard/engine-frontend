module Wizard.Public.Routes exposing (Route(..))


type Route
    = BookReferenceRoute String
    | ForgottenPasswordRoute
    | ForgottenPasswordConfirmationRoute String String
    | LoginRoute (Maybe String)
    | QuestionnaireRoute
    | SignupRoute
    | SignupConfirmationRoute String String
