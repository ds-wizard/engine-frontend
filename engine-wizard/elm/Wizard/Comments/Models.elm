module Wizard.Comments.Models exposing (Model, initialModel)

import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Routes exposing (commentsRouteResolvedFilterId)


type alias Model =
    { commentThreads : Listing.Model QuestionnaireCommentThreadAssigned
    }


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbResolved =
    let
        values =
            [ ( commentsRouteResolvedFilterId, mbResolved )
            ]

        paginationQueryFilters =
            PaginationQueryFilters.create values []
    in
    { commentThreads = Listing.initialModelWithFilters paginationQueryString paginationQueryFilters
    }
