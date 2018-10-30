module Organization.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)
import Utils exposing (validateRegex)


type alias Model =
    { organization : ActionResult Organization
    , savingOrganization : ActionResult String
    , form : Form CustomFormError OrganizationForm
    }


type alias Organization =
    { uuid : String
    , name : String
    , organizationId : String
    }


type alias OrganizationForm =
    { name : String
    , organizationId : String
    }


initialModel : Model
initialModel =
    { organization = Loading
    , savingOrganization = Unset
    , form = initEmptyOrganizationForm
    }


organizationDecoder : Decoder Organization
organizationDecoder =
    decode Organization
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "organizationId" Decode.string


initEmptyOrganizationForm : Form CustomFormError OrganizationForm
initEmptyOrganizationForm =
    Form.initial [] organizationFormValidation


initOrganizationForm : Organization -> Form CustomFormError OrganizationForm
initOrganizationForm organization =
    Form.initial (organizationToFormInitials organization) organizationFormValidation


organizationFormValidation : Validation CustomFormError OrganizationForm
organizationFormValidation =
    Validate.map2 OrganizationForm
        (Validate.field "name" Validate.string)
        (Validate.field "organizationId" (validateRegex "^^(?![.])(?!.*[.]$)[a-zA-Z0-9.]+$"))


organizationToFormInitials : Organization -> List ( String, Field.Field )
organizationToFormInitials organization =
    [ ( "name", Field.string organization.name )
    , ( "organizationId", Field.string organization.organizationId )
    ]


encodeOrganizationForm : String -> OrganizationForm -> Encode.Value
encodeOrganizationForm uuid form =
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "name", Encode.string form.name )
        , ( "organizationId", Encode.string form.organizationId )
        ]
