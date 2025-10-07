module Wizard.Pages.Public.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.Auth.Models
import Wizard.Pages.Public.ForgottenPassword.Models
import Wizard.Pages.Public.ForgottenPasswordConfirmation.Models
import Wizard.Pages.Public.Login.Models
import Wizard.Pages.Public.Routes exposing (Route(..))
import Wizard.Pages.Public.Signup.Models
import Wizard.Pages.Public.SignupConfirmation.Models


type alias Model =
    { authModel : Wizard.Pages.Public.Auth.Models.Model
    , forgottenPasswordModel : Wizard.Pages.Public.ForgottenPassword.Models.Model
    , forgottenPasswordConfirmationModel : Wizard.Pages.Public.ForgottenPasswordConfirmation.Models.Model
    , loginModel : Wizard.Pages.Public.Login.Models.Model
    , signupModel : Wizard.Pages.Public.Signup.Models.Model
    , signupConfirmationModel : Wizard.Pages.Public.SignupConfirmation.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { authModel = Wizard.Pages.Public.Auth.Models.initialModel "" Nothing
    , forgottenPasswordModel = Wizard.Pages.Public.ForgottenPassword.Models.initialModel
    , forgottenPasswordConfirmationModel = Wizard.Pages.Public.ForgottenPasswordConfirmation.Models.initialModel appState "" ""
    , loginModel = Wizard.Pages.Public.Login.Models.initialModel Nothing
    , signupModel = Wizard.Pages.Public.Signup.Models.initialModel appState
    , signupConfirmationModel = Wizard.Pages.Public.SignupConfirmation.Models.initialModel
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        AuthCallback id _ _ mbSessionState ->
            { model | authModel = Wizard.Pages.Public.Auth.Models.initialModel id mbSessionState }

        ForgottenPasswordRoute ->
            { model | forgottenPasswordModel = Wizard.Pages.Public.ForgottenPassword.Models.initialModel }

        ForgottenPasswordConfirmationRoute userId hash ->
            { model | forgottenPasswordConfirmationModel = Wizard.Pages.Public.ForgottenPasswordConfirmation.Models.initialModel appState userId hash }

        LoginRoute mbOriginalUrl ->
            { model | loginModel = Wizard.Pages.Public.Login.Models.initialModel mbOriginalUrl }

        SignupRoute ->
            { model | signupModel = Wizard.Pages.Public.Signup.Models.initialModel appState }

        _ ->
            model
