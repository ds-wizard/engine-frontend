module Registry.Data.Forms.LoginForm exposing (LoginForm, init, validation)

import Form exposing (Form)
import Form.Validate as V exposing (Validation)


type alias LoginForm =
    { organizationId : String
    , token : String
    }


init : Form e LoginForm
init =
    Form.initial [] validation


validation : Validation e LoginForm
validation =
    V.succeed LoginForm
        |> V.andMap (V.field "organizationId" V.string)
        |> V.andMap (V.field "token" V.string)
