module Wizard.Settings.Common.AuthServiceConfigForm exposing
    ( AuthServiceConfigForm
    , configToFormInitials
    , init
    , toEditableAuthServiceConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Field as Field
import Wizard.Common.Form.Validate as V
import Wizard.Settings.Common.EditableAuthServiceConfig exposing (EditableAuthServiceConfig)


type alias AuthServiceConfigForm =
    { id : String
    , name : String
    , url : String
    , clientId : String
    , clientSecret : String
    , styleBackground : Maybe String
    , styleColor : Maybe String
    , styleIcon : Maybe String
    }


init : EditableAuthServiceConfig -> Form CustomFormError AuthServiceConfigForm
init config =
    Form.initial (configToFormInitials config) validation


validation : Validation CustomFormError AuthServiceConfigForm
validation =
    V.succeed AuthServiceConfigForm
        |> V.andMap (V.field "id" V.string)
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "url" V.string)
        |> V.andMap (V.field "clientId" V.string)
        |> V.andMap (V.field "clientSecret" V.string)
        |> V.andMap (V.field "styleBackground" V.maybeString)
        |> V.andMap (V.field "styleColor" V.maybeString)
        |> V.andMap (V.field "styleIcon" V.maybeString)


configToFormInitials : EditableAuthServiceConfig -> List ( String, Field )
configToFormInitials config =
    [ ( "id", Field.string config.id )
    , ( "name", Field.string config.name )
    , ( "url", Field.string config.url )
    , ( "clientId", Field.string config.clientId )
    , ( "clientSecret", Field.string config.clientSecret )
    , ( "styleBackground", Field.maybeString config.style.background )
    , ( "styleColor", Field.maybeString config.style.color )
    , ( "styleIcon", Field.maybeString config.style.icon )
    ]


toEditableAuthServiceConfig : AuthServiceConfigForm -> EditableAuthServiceConfig
toEditableAuthServiceConfig form =
    { id = form.id
    , name = form.name
    , url = form.url
    , clientId = form.clientId
    , clientSecret = form.clientSecret
    , style =
        { background = form.styleBackground
        , color = form.styleColor
        , icon = form.styleIcon
        }
    }
