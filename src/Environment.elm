module Environment exposing (Environment, Msg, getEnvironmentAsYaml, getEnvironmentView, mapEnvironmentEvent, rhdm_trial)

import Console
import Html exposing (Attribute, Html, div, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MODEL


type Environment
    = Rhdm_authoring_ha String
    | Rhdm_authoring String
    | Rhdm_optaweb_trial String
    | Rhdm_production_immutable String
    | Rhdm_trial String (Maybe Console.Console)
    | Rhpam_authoring_ha String
    | Rhpam_authoring String
    | Rhpam_production_immutable String
    | Rhpam_production String
    | Rhpam_trial String


rhdm_authoring_ha : Environment
rhdm_authoring_ha =
    Rhdm_authoring_ha "rhdm-authoring-ha"


rhdm_authoring : Environment
rhdm_authoring =
    Rhdm_authoring "rhdm-authoring"


rhdm_optaweb_trial : Environment
rhdm_optaweb_trial =
    Rhdm_optaweb_trial "rhdm-optaweb-trial"


rhdm_production_immutable : Environment
rhdm_production_immutable =
    Rhdm_production_immutable "rhdm-production-immutable"


rhdm_trial : Environment
rhdm_trial =
    Rhdm_trial "rhdm-trial" Nothing


rhpam_authoring_ha : Environment
rhpam_authoring_ha =
    Rhpam_authoring_ha "rhpam-authoring-ha"


rhpam_authoring : Environment
rhpam_authoring =
    Rhpam_authoring "rhpam-authoring"


rhpam_production_immutable : Environment
rhpam_production_immutable =
    Rhpam_production_immutable "rhpam-production-immutable"


rhpam_production : Environment
rhpam_production =
    Rhpam_production "rhpam-production"


rhpam_trial : Environment
rhpam_trial =
    Rhpam_trial "rhpam-trial"


environments : List Environment
environments =
    [ rhdm_authoring_ha, rhdm_authoring, rhdm_optaweb_trial, rhdm_production_immutable, rhdm_trial, rhpam_authoring_ha, rhpam_authoring, rhpam_production_immutable, rhpam_production, rhpam_trial ]


getEnvironmentName : Environment -> String
getEnvironmentName environment =
    case environment of
        Rhdm_authoring_ha name ->
            name

        Rhdm_authoring name ->
            name

        Rhdm_optaweb_trial name ->
            name

        Rhdm_production_immutable name ->
            name

        Rhdm_trial name _ ->
            name

        Rhpam_authoring_ha name ->
            name

        Rhpam_authoring name ->
            name

        Rhpam_production_immutable name ->
            name

        Rhpam_production name ->
            name

        Rhpam_trial name ->
            name


getEnvironmentFromName : String -> Maybe Environment
getEnvironmentFromName environmentName =
    List.filter (\environment -> getEnvironmentName environment == environmentName) environments |> List.head



-- UPDATE


type Msg
    = SelectEnvironment String
    | ConsoleMsg Console.Msg


mapEnvironmentEvent : Msg -> Environment -> Environment
mapEnvironmentEvent msg environment =
    case msg of
        SelectEnvironment newEnvironmentName ->
            case getEnvironmentFromName newEnvironmentName of
                Nothing ->
                    rhdm_trial

                Just newEnvironment ->
                    newEnvironment

        ConsoleMsg consoleMessage ->
            case environment of
                Rhdm_trial name (Just console) ->
                    Rhdm_trial name (Console.mapConsoleEvent consoleMessage console)

                Rhdm_trial name Nothing ->
                    Rhdm_trial name (Console.mapConsoleEvent consoleMessage Console.emptyConsole)

                _ ->
                    environment



-- VIEW


getEnvironmentView : (Msg -> msg) -> Environment -> List (Html msg)
getEnvironmentView msg environment =
    [ div [] [ text "Environment: ", select [ onInput (SelectEnvironment >> msg) ] (List.map (toEnvironmentOption environment) environments) ] ]
        ++ getConsoleView msg environment


getConsoleView : (Msg -> msg) -> Environment -> List (Html msg)
getConsoleView msg environment =
    case environment of
        Rhdm_trial _ (Just console) ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhdm_trial _ Nothing ->
            Console.getConsoleViewEmpty (ConsoleMsg >> msg)

        _ ->
            []


toEnvironmentOption : Environment -> Environment -> Html msg
toEnvironmentOption selectedEnvironment environment =
    option [ Html.Attributes.selected (getEnvironmentName selectedEnvironment == getEnvironmentName environment), value (getEnvironmentName environment) ] [ text (getEnvironmentName environment) ]



-- YAML


getEnvironmentAsYaml : Environment -> String
getEnvironmentAsYaml environment =
    "  environment: "
        ++ getEnvironmentName environment
        ++ "\n"
        ++ getObjectsAsYaml environment


getObjectsAsYaml : Environment -> String
getObjectsAsYaml environment =
    case getConsoleAsYaml environment of
        "" ->
            ""

        contentOfObjects ->
            "  objects:\n" ++ contentOfObjects


getConsoleAsYaml : Environment -> String
getConsoleAsYaml environment =
    case environment of
        Rhdm_trial _ (Just console) ->
            Console.getConsoleAsYaml console

        _ ->
            ""
