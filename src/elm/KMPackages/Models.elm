module KMPackages.Models exposing (..)

import KMPackages.Detail.Models
import KMPackages.Import.Models
import KMPackages.Index.Models
import KMPackages.Routing exposing (Route(..))


type alias Model =
    { detailModel : KMPackages.Detail.Models.Model
    , importModel : KMPackages.Import.Models.Model
    , indexModel : KMPackages.Index.Models.Model
    }


initialModel : Model
initialModel =
    { detailModel = KMPackages.Detail.Models.initialModel
    , importModel = KMPackages.Import.Models.initialModel
    , indexModel = KMPackages.Index.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        Detail _ _ ->
            { model | detailModel = KMPackages.Detail.Models.initialModel }

        Import ->
            { model | importModel = KMPackages.Import.Models.initialModel }

        Index ->
            { model | indexModel = KMPackages.Index.Models.initialModel }
