module Wizard.Public.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Public.Auth.Models
import Wizard.Public.ForgottenPassword.Models
import Wizard.Public.ForgottenPasswordConfirmation.Models
import Wizard.Public.Login.Models
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Public.Signup.Models
import Wizard.Public.SignupConfirmation.Models


type alias Model =
    { authModel : Wizard.Public.Auth.Models.Model
    , forgottenPasswordModel : Wizard.Public.ForgottenPassword.Models.Model
    , forgottenPasswordConfirmationModel : Wizard.Public.ForgottenPasswordConfirmation.Models.Model
    , loginModel : Wizard.Public.Login.Models.Model
    , signupModel : Wizard.Public.Signup.Models.Model
    , signupConfirmationModel : Wizard.Public.SignupConfirmation.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { authModel = Wizard.Public.Auth.Models.initialModel "" Nothing
    , forgottenPasswordModel = Wizard.Public.ForgottenPassword.Models.initialModel
    , forgottenPasswordConfirmationModel = Wizard.Public.ForgottenPasswordConfirmation.Models.initialModel appState "" ""
    , loginModel = Wizard.Public.Login.Models.initialModel Nothing
    , signupModel = Wizard.Public.Signup.Models.initialModel appState
    , signupConfirmationModel = Wizard.Public.SignupConfirmation.Models.initialModel
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        AuthCallback id _ _ mbSessionState ->
            { model | authModel = Wizard.Public.Auth.Models.initialModel id mbSessionState }

        ForgottenPasswordRoute ->
            { model | forgottenPasswordModel = Wizard.Public.ForgottenPassword.Models.initialModel }

        ForgottenPasswordConfirmationRoute userId hash ->
            { model | forgottenPasswordConfirmationModel = Wizard.Public.ForgottenPasswordConfirmation.Models.initialModel appState userId hash }

        LoginRoute mbOriginalUrl ->
            { model | loginModel = Wizard.Public.Login.Models.initialModel mbOriginalUrl }

        SignupRoute ->
            { model | signupModel = Wizard.Public.Signup.Models.initialModel appState }

        _ ->
            model
