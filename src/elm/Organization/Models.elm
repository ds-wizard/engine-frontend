module Organization.Models exposing (..)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)
import Utils exposing (FormResult(..), validateRegex)


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
    , groupId : String
    }


organizationDecoder : Decoder Organization
organizationDecoder =
    decode Organization
        |> required "name" Decode.string
        |> required "groupId" Decode.string


type alias OrganizationForm =
    { name : String
    , groupId : String
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
        (Validate.field "groupId" (validateRegex "^[a-zA-Z0-9.]+$"))


organizationToFormInitials : Organization -> List ( String, Field.Field )
organizationToFormInitials organization =
    [ ( "name", Field.string organization.name )
    , ( "groupId", Field.string organization.groupId )
    ]


encodeOrganizationForm : OrganizationForm -> Encode.Value
encodeOrganizationForm form =
    Encode.object
        [ ( "name", Encode.string form.name )
        , ( "groupId", Encode.string form.groupId )
        ]
