module Environment exposing (Environment, Msg, getEnvironmentAsYaml, getEnvironmentView, mapEnvironmentEvent, rhdm_trial)

import Console
import Html exposing (Attribute, Html, div, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Servers
import YamlUtils



-- MODEL


type Environment
    = Rhdm_authoring_ha String
    | Rhdm_authoring String
    | Rhdm_production_immutable String
    | Rhdm_trial String (Maybe Console.Console) (Maybe Servers.Servers)
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


rhdm_production_immutable : Environment
rhdm_production_immutable =
    Rhdm_production_immutable "rhdm-production-immutable"


rhdm_trial : Environment
rhdm_trial =
    Rhdm_trial "rhdm-trial" Nothing Nothing


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
    [ rhdm_authoring_ha, rhdm_authoring, rhdm_production_immutable, rhdm_trial, rhpam_authoring_ha, rhpam_authoring, rhpam_production_immutable, rhpam_production, rhpam_trial ]


getEnvironmentName : Environment -> String
getEnvironmentName environment =
    case environment of
        Rhdm_authoring_ha name ->
            name

        Rhdm_authoring name ->
            name

        Rhdm_production_immutable name ->
            name

        Rhdm_trial name _ _ ->
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
    | ServersMsg Servers.Msg


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
                Rhdm_trial name (Just console) servers ->
                    Rhdm_trial name (Console.mapConsoleEvent consoleMessage console) servers

                Rhdm_trial name Nothing servers ->
                    Rhdm_trial name (Console.mapConsoleEvent consoleMessage Console.emptyConsole) servers

                _ ->
                    environment

        ServersMsg serversMessage ->
            case environment of
                Rhdm_trial name console (Just servers) ->
                    Rhdm_trial name console (Servers.mapServersEvent serversMessage servers)

                Rhdm_trial name console Nothing ->
                    Rhdm_trial name console (Servers.mapServersEvent serversMessage { servers = [] })

                _ ->
                    environment



-- VIEW


getEnvironmentView : (Msg -> msg) -> Environment -> List (Html msg)
getEnvironmentView msg environment =
    [ div [] [ text "Environment: ", select [ onInput (SelectEnvironment >> msg) ] (List.map (toEnvironmentOption environment) environments) ] ]
        ++ getConsoleView msg environment
        ++ getServersView msg environment


getConsoleView : (Msg -> msg) -> Environment -> List (Html msg)
getConsoleView msg environment =
    case environment of
        Rhdm_trial _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhdm_trial _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole

        _ ->
            []


toEnvironmentOption : Environment -> Environment -> Html msg
toEnvironmentOption selectedEnvironment environment =
    option [ Html.Attributes.selected (getEnvironmentName selectedEnvironment == getEnvironmentName environment), value (getEnvironmentName environment) ] [ text (getEnvironmentName environment) ]


getServersView : (Msg -> msg) -> Environment -> List (Html msg)
getServersView msg environment =
    case environment of
        Rhdm_trial _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhdm_trial _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }

        _ ->
            []



-- YAML


getEnvironmentAsYaml : Environment -> Int -> String
getEnvironmentAsYaml environment intendation =
    YamlUtils.getNameAndValueWithIntendation "environment" (getEnvironmentName environment) intendation
        ++ getObjectsAsYaml environment intendation


getObjectsAsYaml : Environment -> Int -> String
getObjectsAsYaml environment intendation =
    case getConsoleAsYaml environment (intendation + 1) ++ getServersAsYaml environment (intendation + 1) of
        "" ->
            ""

        contentOfObjects ->
            YamlUtils.getNameWithIntendation "objects" intendation
                ++ contentOfObjects


getConsoleAsYaml : Environment -> Int -> String
getConsoleAsYaml environment intendation =
    case environment of
        Rhdm_trial _ (Just console) _ ->
            Console.getConsoleAsYaml console intendation

        _ ->
            ""


getServersAsYaml : Environment -> Int -> String
getServersAsYaml environment intendation =
    case environment of
        Rhdm_trial _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        _ ->
            ""
