module Organization.Common.OrganizationForm exposing
    ( OrganizationForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Organization.Common.Organization exposing (Organization)
import Utils exposing (validateRegex)


type alias OrganizationForm =
    { name : String
    , organizationId : String
    }


initEmpty : Form CustomFormError OrganizationForm
initEmpty =
    Form.initial [] validation


init : Organization -> Form CustomFormError OrganizationForm
init organization =
    Form.initial (organizationToFormInitials organization) validation


validation : Validation CustomFormError OrganizationForm
validation =
    Validate.map2 OrganizationForm
        (Validate.field "name" Validate.string)
        (Validate.field "organizationId" (validateRegex "^^(?![.])(?!.*[.]$)[a-zA-Z0-9.]+$"))


organizationToFormInitials : Organization -> List ( String, Field.Field )
organizationToFormInitials organization =
    [ ( "name", Field.string organization.name )
    , ( "organizationId", Field.string organization.organizationId )
    ]


encode : String -> OrganizationForm -> E.Value
encode uuid form =
    E.object
        [ ( "uuid", E.string uuid )
        , ( "name", E.string form.name )
        , ( "organizationId", E.string form.organizationId )
        ]
