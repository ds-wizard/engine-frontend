module Registry2.Data.Forms.OrganizationForm exposing
    ( OrganizationForm
    , encode
    , init
    , initFromOrganization
    , validation
    )

import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Registry2.Api.Models.Organization exposing (Organization)


type alias OrganizationForm =
    { name : String
    , description : String
    , email : String
    }


initFromOrganization : Organization -> Form e OrganizationForm
initFromOrganization organization =
    initOrganizationForm
        [ ( "name", Field.string organization.name )
        , ( "description", Field.string organization.description )
        , ( "email", Field.string organization.email )
        ]


init : Form e OrganizationForm
init =
    initOrganizationForm []


validation : Validation e OrganizationForm
validation =
    V.succeed OrganizationForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.string)
        |> V.andMap (V.field "email" V.email)


initOrganizationForm : List ( String, Field ) -> Form e OrganizationForm
initOrganizationForm initials =
    Form.initial initials validation


encode : OrganizationForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "description", E.string form.description )
        , ( "email", E.string form.email )
        ]
