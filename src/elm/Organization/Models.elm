module Organization.Models exposing (..)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)
import Utils exposing (FormResult(..))


type alias Model =
    { loading : Bool
    , loadingError : String
    , form : Form () OrganizationForm
    , saving : Bool
    , result : FormResult
    }


initialModel : Model
initialModel =
    { loading = True
    , loadingError = ""
    , form = initEmptyOrganizationForm
    , saving = False
    , result = None
    }


type alias Organization =
    { name : String
    , namespace : String
    }


organizationDecoder : Decoder Organization
organizationDecoder =
    decode Organization
        |> required "name" Decode.string
        |> required "namespace" Decode.string


type alias OrganizationForm =
    { name : String
    , namespace : String
    }


initEmptyOrganizationForm : Form () OrganizationForm
initEmptyOrganizationForm =
    Form.initial [] organizationFormValidation


initOrganizationForm : Organization -> Form () OrganizationForm
initOrganizationForm organization =
    Form.initial (organizationToFormInitials organization) organizationFormValidation


organizationFormValidation : Validation () OrganizationForm
organizationFormValidation =
    Validate.map2 OrganizationForm
        (Validate.field "name" Validate.string)
        (Validate.field "namespace" Validate.string)


organizationToFormInitials : Organization -> List ( String, Field.Field )
organizationToFormInitials organization =
    [ ( "name", Field.string organization.name )
    , ( "namespace", Field.string organization.namespace )
    ]


encodeOrganizationForm : OrganizationForm -> Encode.Value
encodeOrganizationForm form =
    Encode.object
        [ ( "name", Encode.string form.name )
        , ( "namespace", Encode.string form.namespace )
        ]
