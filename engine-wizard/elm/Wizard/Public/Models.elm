module Wizard.Public.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Public.Auth.Models
import Wizard.Public.BookReference.Models
import Wizard.Public.ForgottenPassword.Models
import Wizard.Public.ForgottenPasswordConfirmation.Models
import Wizard.Public.Login.Models
import Wizard.Public.Questionnaire.Models
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Public.Signup.Models
import Wizard.Public.SignupConfirmation.Models


type alias Model =
    { authModel : Wizard.Public.Auth.Models.Model
    , bookReferenceModel : Wizard.Public.BookReference.Models.Model
    , forgottenPasswordModel : Wizard.Public.ForgottenPassword.Models.Model
    , forgottenPasswordConfirmationModel : Wizard.Public.ForgottenPasswordConfirmation.Models.Model
    , loginModel : Wizard.Public.Login.Models.Model
    , questionnaireModel : Wizard.Public.Questionnaire.Models.Model
    , signupModel : Wizard.Public.Signup.Models.Model
    , signupConfirmationModel : Wizard.Public.SignupConfirmation.Models.Model
    }


initialModel : Model
initialModel =
    { authModel = Wizard.Public.Auth.Models.initialModel
    , bookReferenceModel = Wizard.Public.BookReference.Models.initialModel
    , forgottenPasswordModel = Wizard.Public.ForgottenPassword.Models.initialModel
    , forgottenPasswordConfirmationModel = Wizard.Public.ForgottenPasswordConfirmation.Models.initialModel "" ""
    , loginModel = Wizard.Public.Login.Models.initialModel Nothing
    , questionnaireModel = Wizard.Public.Questionnaire.Models.initialModel
    , signupModel = Wizard.Public.Signup.Models.initialModel
    , signupConfirmationModel = Wizard.Public.SignupConfirmation.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        AuthCallback _ _ _ ->
            { model | authModel = Wizard.Public.Auth.Models.initialModel }

        BookReferenceRoute _ ->
            { model | bookReferenceModel = Wizard.Public.BookReference.Models.initialModel }

        ForgottenPasswordRoute ->
            { model | forgottenPasswordModel = Wizard.Public.ForgottenPassword.Models.initialModel }

        ForgottenPasswordConfirmationRoute userId hash ->
            { model | forgottenPasswordConfirmationModel = Wizard.Public.ForgottenPasswordConfirmation.Models.initialModel userId hash }

        LoginRoute mbOriginalUrl ->
            { model | loginModel = Wizard.Public.Login.Models.initialModel mbOriginalUrl }

        QuestionnaireRoute ->
            { model | questionnaireModel = Wizard.Public.Questionnaire.Models.initialModel }

        SignupRoute ->
            { model | signupModel = Wizard.Public.Signup.Models.initialModel }

        _ ->
            model
