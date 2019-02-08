# 1.3.0

- There were no changes for this release


# 1.2.0

## Features

- Add alert when leaving editor with unsaved changes
- Split user menu to separate Help

## Fixes

- Fix user delete modal email overflow

## Other

- Update questionnaire replies structure



# 1.1.0

## Features

- Add redirect to a questionnaire after it is created
- Add information whether the questionnaire is private or public
- Data Steward can now import/export packages
- Update terminology in menu and buttons to be more descriptive
- Remove default item for list questions in questionnaire

## Fixes

- Fix that itemTitle could not be changed
- Fix event generation when saving in editor (causing last event to be lost sometimes)

## Other

- Migrate to Elm 0.19


# 1.0.0

## Features

- Option to create private questionnaire
- Information about current version of the client and server
- Latest package version is now visible in KM packages list

## Fixes

- Fixed KM migration view

## Other

- Travis build now push only latest, develop and tagged images into Docker Hub



# 0.6.2

## Changes

- PDF export button returned


# 0.6.1

## Changes

- Temporarily removed PDF export button


# 0.6.0

## Features

- References types
- Metrics & Indications
- Phases
- Improved questionnaire
    - Summary report
    - All related entities displayed by the question
    - Unsaved changes warning
    - Clear answer button
    - Answered questions indication for each chapter with respect to phase

## Fixes

- Question text is now optional

## Other

- Question short uuid was removed and reference with proper type is used instead


# 0.5.0

## Features

- Reworked Knowledge Model Editor
    - Better navigation through the Knowledge Model
    - More accurate generating of events
    - Fixed bugs with list of items question type

## Fixes

- Fixed server error parsing when there were no `fieldErrors`

## Other

- Make fill questionnaire action more significant
- Add more info in the welcome page


# 0.4.0

## Features

- Feedback
- More DMP export formats
- Book header for book references

## Fixes

- Fix flag colors for KM editor states
- Tables are now sorted


# 0.3.0

## Features

- Public book references
- Public questionnaire demo

## Other

- Split permision `PM_PERM` to `PM_READ_PERM` and `PM_WRITE_PERM`
- Logout user when the server respond with 401
- Privacy link added to the signup form
- Cache busting


# 0.2.1

## Fixes

- Fix a bug when switching the chapter in questionnaire without saving it first caused loosing the replies data


# 0.2.0

## Data structures

- New events data structure
- Change structure of questionnaire form values

## User Interface

- Bootstrap 4
- Collapsible menu
- Quick links for creating DS Planner and KM Editor from package
- DMP export links

## Fixes

- Fix getServerError to use default message in case of empty message

## Other

- KMEditor module new structure


# 0.1.0

- Initial version of DSW Client
    - Basic Organization and User management
    - Knowlege Model Editor
    - Knowledge Model Packages management
    - First version of DS Planner
