module Public.Models exposing (Model, initLocalModel, initialModel)

import Public.BookReference.Models
import Public.ForgottenPassword.Models
import Public.ForgottenPasswordConfirmation.Models
import Public.Login.Models
import Public.Questionnaire.Models
import Public.Routes exposing (Route(..))
import Public.Signup.Models
import Public.SignupConfirmation.Models


type alias Model =
    { bookReferenceModel : Public.BookReference.Models.Model
    , forgottenPasswordModel : Public.ForgottenPassword.Models.Model
    , forgottenPasswordConfirmationModel : Public.ForgottenPasswordConfirmation.Models.Model
    , loginModel : Public.Login.Models.Model
    , questionnaireModel : Public.Questionnaire.Models.Model
    , signupModel : Public.Signup.Models.Model
    , signupConfirmationModel : Public.SignupConfirmation.Models.Model
    }


initialModel : Model
initialModel =
    { bookReferenceModel = Public.BookReference.Models.initialModel
    , forgottenPasswordModel = Public.ForgottenPassword.Models.initialModel
    , forgottenPasswordConfirmationModel = Public.ForgottenPasswordConfirmation.Models.initialModel "" ""
    , loginModel = Public.Login.Models.initialModel
    , questionnaireModel = Public.Questionnaire.Models.initialModel
    , signupModel = Public.Signup.Models.initialModel
    , signupConfirmationModel = Public.SignupConfirmation.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        BookReferenceRoute _ ->
            { model | bookReferenceModel = Public.BookReference.Models.initialModel }

        ForgottenPasswordRoute ->
            { model | forgottenPasswordModel = Public.ForgottenPassword.Models.initialModel }

        ForgottenPasswordConfirmationRoute userId hash ->
            { model | forgottenPasswordConfirmationModel = Public.ForgottenPasswordConfirmation.Models.initialModel userId hash }

        LoginRoute ->
            { model | loginModel = Public.Login.Models.initialModel }

        QuestionnaireRoute ->
            { model | questionnaireModel = Public.Questionnaire.Models.initialModel }

        SignupRoute ->
            { model | signupModel = Public.Signup.Models.initialModel }

        _ ->
            model
