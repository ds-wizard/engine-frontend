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
