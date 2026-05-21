module Wizard.Pages.Settings.OpenIdDetail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Uuid exposing (Uuid)
import Wizard.Api.Models.OpenIdClientDetail exposing (OpenIdClientDetail)
import Wizard.Components.CopyableCodeBlock as CopyableCodeBlock
import Wizard.Pages.Settings.Common.Forms.OpenIdCreateForm as OpenIdClientForm exposing (OpenIdClientForm)


type alias Model =
    { uuid : Uuid
    , openIdClient : ActionResult OpenIdClientDetail
    , form : Form FormError OpenIdClientForm
    , formRemoved : Bool
    , savingOpenId : ActionResult ()
    , callbackUrlCodeBlockState : CopyableCodeBlock.Model
    , logoutUrlCodeBlockState : CopyableCodeBlock.Model
    , advancedConfigExpanded : Bool
    }


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , openIdClient = ActionResult.Loading
    , form = OpenIdClientForm.initEmpty
    , formRemoved = False
    , savingOpenId = ActionResult.Unset
    , callbackUrlCodeBlockState = CopyableCodeBlock.initialModel
    , logoutUrlCodeBlockState = CopyableCodeBlock.initialModel
    , advancedConfigExpanded = False
    }
