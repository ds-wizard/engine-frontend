module Organization.Models exposing (..)

{-|


# Types

@docs Model, Organization, OrganizationForm


# Model helpers

@docs initialModel


# Organization helpers

@docs organizationDecoder


# OrganizationForm helpers

@docs initEmptyOrganizationForm, initOrganizationForm, organizationFormValidation, organizationToFormInitials, encodeOrganizationForm

-}

import Common.Form exposing (CustomFormError)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)
import Utils exposing (validateRegex)


{-| -}
type alias Model =
    { organization : ActionResult Organization
    , savingOrganization : ActionResult String
    , form : Form CustomFormError OrganizationForm
    }


{-| -}
type alias Organization =
    { uuid : String
    , name : String
    , groupId : String
    }


{-| -}
type alias OrganizationForm =
    { name : String
    , groupId : String
    }


{-| -}
initialModel : Model
initialModel =
    { organization = Loading
    , savingOrganization = Unset
    , form = initEmptyOrganizationForm
    }


{-| -}
organizationDecoder : Decoder Organization
organizationDecoder =
    decode Organization
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "groupId" Decode.string


{-| -}
initEmptyOrganizationForm : Form CustomFormError OrganizationForm
initEmptyOrganizationForm =
    Form.initial [] organizationFormValidation


{-| -}
initOrganizationForm : Organization -> Form CustomFormError OrganizationForm
initOrganizationForm organization =
    Form.initial (organizationToFormInitials organization) organizationFormValidation


{-| -}
organizationFormValidation : Validation CustomFormError OrganizationForm
organizationFormValidation =
    Validate.map2 OrganizationForm
        (Validate.field "name" Validate.string)
        (Validate.field "groupId" (validateRegex "^^(?![.])(?!.*[.]$)[a-zA-Z0-9.]+$"))


{-| -}
organizationToFormInitials : Organization -> List ( String, Field.Field )
organizationToFormInitials organization =
    [ ( "name", Field.string organization.name )
    , ( "groupId", Field.string organization.groupId )
    ]


{-| -}
encodeOrganizationForm : String -> OrganizationForm -> Encode.Value
encodeOrganizationForm uuid form =
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "name", Encode.string form.name )
        , ( "groupId", Encode.string form.groupId )
        ]
