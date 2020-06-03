module WizardResearch.Route.ProjectRoute exposing
    ( ProjectRoute(..)
    , toString
    )


type ProjectRoute
    = Overview
    | Planning
    | Starred
    | Metrics
    | Documents
    | Settings


toString : ProjectRoute -> String
toString projectRoute =
    case projectRoute of
        Overview ->
            ""

        Planning ->
            "planning"

        Starred ->
            "starred"

        Metrics ->
            "metrics"

        Documents ->
            "documents"

        Settings ->
            "settings"
