module Wizard.Settings.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Settings.Affiliation.Models
import Wizard.Settings.Auth.Models
import Wizard.Settings.Client.Models
import Wizard.Settings.Features.Models
import Wizard.Settings.Info.Models
import Wizard.Settings.Organization.Models
import Wizard.Settings.Routes exposing (Route(..))


type alias Model =
    { affiliationModel : Wizard.Settings.Affiliation.Models.Model
    , authModel : Wizard.Settings.Auth.Models.Model
    , clientModel : Wizard.Settings.Client.Models.Model
    , featuresModel : Wizard.Settings.Features.Models.Model
    , infoModel : Wizard.Settings.Info.Models.Model
    , organizationModel : Wizard.Settings.Organization.Models.Model
    }


initialModel : Model
initialModel =
    { affiliationModel = Wizard.Settings.Affiliation.Models.initialModel
    , authModel = Wizard.Settings.Auth.Models.initialModel
    , clientModel = Wizard.Settings.Client.Models.initialModel
    , featuresModel = Wizard.Settings.Features.Models.initialModel
    , infoModel = Wizard.Settings.Info.Models.initialModel
    , organizationModel = Wizard.Settings.Organization.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        AuthRoute ->
            { model | authModel = Wizard.Settings.Auth.Models.initialModel }

        AffiliationRoute ->
            { model | affiliationModel = Wizard.Settings.Affiliation.Models.initialModel }

        ClientRoute ->
            { model | clientModel = Wizard.Settings.Client.Models.initialModel }

        FeaturesRoute ->
            { model | featuresModel = Wizard.Settings.Features.Models.initialModel }

        InfoRoute ->
            { model | infoModel = Wizard.Settings.Info.Models.initialModel }

        OrganizationRoute ->
            { model | organizationModel = Wizard.Settings.Organization.Models.initialModel }
