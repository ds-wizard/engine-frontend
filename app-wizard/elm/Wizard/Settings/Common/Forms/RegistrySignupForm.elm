module Wizard.Settings.Common.Forms.RegistrySignupForm exposing
    ( RegistrySignupForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Form.Validate as V
import Wizard.Api.Models.BootstrapConfig.OrganizationConfig exposing (OrganizationConfig)
import Wizard.Common.AppState exposing (AppState)


type alias RegistrySignupForm =
    { organizationId : String
    , name : String
    , description : String
    , email : String
    }


initEmpty : AppState -> Form FormError RegistrySignupForm
initEmpty appState =
    Form.initial [] (validation appState)


init : AppState -> OrganizationConfig -> Form FormError RegistrySignupForm
init appState config =
    let
        email =
            appState.config.user
                |> Maybe.map .email
                |> Maybe.withDefault ""

        initials =
            [ ( "organizationId", Field.string config.organizationId )
            , ( "name", Field.string config.name )
            , ( "description", Field.string config.description )
            , ( "email", Field.string email )
            ]
    in
    Form.initial initials (validation appState)


validation : AppState -> Validation FormError RegistrySignupForm
validation appState =
    V.succeed RegistrySignupForm
        |> V.andMap (V.field "organizationId" (V.organizationId appState))
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.string)
        |> V.andMap (V.field "email" V.email)


encode : RegistrySignupForm -> E.Value
encode form =
    E.object
        [ ( "organizationId", E.string form.organizationId )
        , ( "name", E.string form.name )
        , ( "description", E.string form.description )
        , ( "email", E.string form.email )
        ]
