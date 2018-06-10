module Public.Models exposing (..)

import Public.BookReference.Models
import Public.ForgottenPassword.Models
import Public.ForgottenPasswordConfirmation.Models
import Public.Login.Models
import Public.Routing exposing (Route(..))
import Public.Signup.Models
import Public.SignupConfirmation.Models


type alias Model =
    { bookReferenceModel : Public.BookReference.Models.Model
    , forgottenPasswordModel : Public.ForgottenPassword.Models.Model
    , forgottenPasswordConfirmationModel : Public.ForgottenPasswordConfirmation.Models.Model
    , loginModel : Public.Login.Models.Model
    , signupModel : Public.Signup.Models.Model
    , signupConfirmationModel : Public.SignupConfirmation.Models.Model
    }


initialModel : Model
initialModel =
    { bookReferenceModel = Public.BookReference.Models.initialModel
    , forgottenPasswordModel = Public.ForgottenPassword.Models.initialModel
    , forgottenPasswordConfirmationModel = Public.ForgottenPasswordConfirmation.Models.initialModel "" ""
    , loginModel = Public.Login.Models.initialModel
    , signupModel = Public.Signup.Models.initialModel
    , signupConfirmationModel = Public.SignupConfirmation.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        BookReference uuid ->
            { model | bookReferenceModel = Public.BookReference.Models.initialModel }

        ForgottenPassword ->
            { model | forgottenPasswordModel = Public.ForgottenPassword.Models.initialModel }

        ForgottenPasswordConfirmation userId hash ->
            { model | forgottenPasswordConfirmationModel = Public.ForgottenPasswordConfirmation.Models.initialModel userId hash }

        Login ->
            { model | loginModel = Public.Login.Models.initialModel }

        Signup ->
            { model | signupModel = Public.Signup.Models.initialModel }

        _ ->
            model
