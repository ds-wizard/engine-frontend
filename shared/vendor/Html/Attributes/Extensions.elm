module Html.Attributes.Extensions exposing
    ( dataCy
    , dataTour
    , selectDataTour
    )

import Html
import Html.Attributes exposing (attribute)


dataCy : String -> Html.Attribute msg
dataCy =
    attribute "data-cy"


dataTour : String -> Html.Attribute msg
dataTour =
    attribute "data-tour"


selectDataTour : String -> Maybe String
selectDataTour tourId =
    Just ("[data-tour='" ++ tourId ++ "']")
