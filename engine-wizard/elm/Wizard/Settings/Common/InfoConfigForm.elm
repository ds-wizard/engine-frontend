module Wizard.Settings.Common.InfoConfigForm exposing
    ( InfoConfigForm
    , init
    , initEmpty
    , toEditableInfoConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Field as Field
import Wizard.Common.Form.Validate as V
import Wizard.Settings.Common.EditableInfoConfig exposing (EditableInfoConfig)


type alias InfoConfigForm =
    { welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    , loginInfo : Maybe String
    }


initEmpty : Form CustomFormError InfoConfigForm
initEmpty =
    Form.initial [] validation


init : EditableInfoConfig -> Form CustomFormError InfoConfigForm
init config =
    Form.initial (configToFormInitials config) validation


validation : Validation CustomFormError InfoConfigForm
validation =
    V.succeed InfoConfigForm
        |> V.andMap (V.field "welcomeInfo" V.maybeString)
        |> V.andMap (V.field "welcomeWarning" V.maybeString)
        |> V.andMap (V.field "loginInfo" V.maybeString)


configToFormInitials : EditableInfoConfig -> List ( String, Field.Field )
configToFormInitials config =
    [ ( "welcomeInfo", Field.maybeString config.welcomeInfo )
    , ( "welcomeWarning", Field.maybeString config.welcomeWarning )
    , ( "loginInfo", Field.maybeString config.loginInfo )
    ]


toEditableInfoConfig : InfoConfigForm -> EditableInfoConfig
toEditableInfoConfig form =
    { welcomeInfo = form.welcomeInfo
    , welcomeWarning = form.welcomeWarning
    , loginInfo = form.loginInfo
    }
