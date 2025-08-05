module Wizard.Settings.Common.Forms.OpenIDServiceConfigForm exposing
    ( OpenIDServiceConfigForm
    , configToFormInitials
    , toEditableOpenIDServiceConfig
    , validation
    )

import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig as EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Wizard.Common.AppState exposing (AppState)


type alias OpenIDServiceConfigForm =
    { id : String
    , name : String
    , url : String
    , clientId : String
    , clientSecret : String
    , parameters : List EditableOpenIDServiceConfig.Parameter
    , styleBackground : Maybe String
    , styleColor : Maybe String
    , styleIcon : Maybe String
    }


validation : AppState -> Validation FormError OpenIDServiceConfigForm
validation appState =
    let
        validateParameter =
            V.succeed EditableOpenIDServiceConfig.Parameter
                |> V.andMap (V.field "name" V.string)
                |> V.andMap (V.field "value" V.string)
    in
    V.succeed OpenIDServiceConfigForm
        |> V.andMap (V.field "id" (V.authenticationServiceId appState))
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "url" V.string)
        |> V.andMap (V.field "clientId" V.string)
        |> V.andMap (V.field "clientSecret" V.string)
        |> V.andMap (V.field "parameters" (V.list validateParameter))
        |> V.andMap (V.field "styleBackground" V.maybeString)
        |> V.andMap (V.field "styleColor" V.maybeString)
        |> V.andMap (V.field "styleIcon" V.maybeString)


configToFormInitials : EditableOpenIDServiceConfig -> List ( String, Field )
configToFormInitials config =
    let
        parameters =
            config.parameters
                |> List.map
                    (\p ->
                        Field.group
                            [ ( "name", Field.string p.name )
                            , ( "value", Field.string p.value )
                            ]
                    )
    in
    [ ( "id", Field.string config.id )
    , ( "name", Field.string config.name )
    , ( "url", Field.string config.url )
    , ( "clientId", Field.string config.clientId )
    , ( "clientSecret", Field.string config.clientSecret )
    , ( "parameters", Field.list parameters )
    , ( "styleBackground", Field.maybeString config.style.background )
    , ( "styleColor", Field.maybeString config.style.color )
    , ( "styleIcon", Field.maybeString config.style.icon )
    ]


toEditableOpenIDServiceConfig : OpenIDServiceConfigForm -> EditableOpenIDServiceConfig
toEditableOpenIDServiceConfig form =
    { id = form.id
    , name = form.name
    , url = form.url
    , clientId = form.clientId
    , clientSecret = form.clientSecret
    , parameters = form.parameters
    , style =
        { background = form.styleBackground
        , color = form.styleColor
        , icon = form.styleIcon
        }
    }
