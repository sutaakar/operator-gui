module Environment exposing (Environment, Msg, getEnvironmentAsYaml, getEnvironmentView, mapEnvironmentEvent, rhdm_trial)

import Console
import Html exposing (Attribute, Html, div, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Servers
import YamlUtils



-- MODEL


type Environment
    = Rhdm_authoring_ha String (Maybe Console.Console) (Maybe Servers.Servers)
    | Rhdm_authoring String (Maybe Console.Console) (Maybe Servers.Servers)
    | Rhdm_production_immutable String (Maybe Console.Console) (Maybe Servers.Servers)
    | Rhdm_trial String (Maybe Console.Console) (Maybe Servers.Servers)
    | Rhpam_authoring_ha String (Maybe Console.Console) (Maybe Servers.Servers)
    | Rhpam_authoring String (Maybe Console.Console) (Maybe Servers.Servers)
    | Rhpam_production_immutable String (Maybe Console.Console) (Maybe Servers.Servers)
    | Rhpam_production String (Maybe Console.Console) (Maybe Servers.Servers)
    | Rhpam_trial String (Maybe Console.Console) (Maybe Servers.Servers)


rhdm_authoring_ha : Environment
rhdm_authoring_ha =
    Rhdm_authoring_ha "rhdm-authoring-ha" Nothing Nothing


rhdm_authoring : Environment
rhdm_authoring =
    Rhdm_authoring "rhdm-authoring" Nothing Nothing


rhdm_production_immutable : Environment
rhdm_production_immutable =
    Rhdm_production_immutable "rhdm-production-immutable" Nothing Nothing


rhdm_trial : Environment
rhdm_trial =
    Rhdm_trial "rhdm-trial" Nothing Nothing


rhpam_authoring_ha : Environment
rhpam_authoring_ha =
    Rhpam_authoring_ha "rhpam-authoring-ha" Nothing Nothing


rhpam_authoring : Environment
rhpam_authoring =
    Rhpam_authoring "rhpam-authoring" Nothing Nothing


rhpam_production_immutable : Environment
rhpam_production_immutable =
    Rhpam_production_immutable "rhpam-production-immutable" Nothing Nothing


rhpam_production : Environment
rhpam_production =
    Rhpam_production "rhpam-production" Nothing Nothing


rhpam_trial : Environment
rhpam_trial =
    Rhpam_trial "rhpam-trial" Nothing Nothing


environments : List Environment
environments =
    [ rhdm_authoring_ha, rhdm_authoring, rhdm_production_immutable, rhdm_trial, rhpam_authoring_ha, rhpam_authoring, rhpam_production_immutable, rhpam_production, rhpam_trial ]


getEnvironmentName : Environment -> String
getEnvironmentName environment =
    case environment of
        Rhdm_authoring_ha name _ _ ->
            name

        Rhdm_authoring name _ _ ->
            name

        Rhdm_production_immutable name _ _ ->
            name

        Rhdm_trial name _ _ ->
            name

        Rhpam_authoring_ha name _ _ ->
            name

        Rhpam_authoring name _ _ ->
            name

        Rhpam_production_immutable name _ _ ->
            name

        Rhpam_production name _ _ ->
            name

        Rhpam_trial name _ _ ->
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

                Rhdm_authoring name (Just console) servers ->
                    Rhdm_authoring name (Console.mapConsoleEvent consoleMessage console) servers

                Rhdm_authoring name Nothing servers ->
                    Rhdm_authoring name (Console.mapConsoleEvent consoleMessage Console.emptyConsole) servers

                Rhdm_authoring_ha name (Just console) servers ->
                    Rhdm_authoring_ha name (Console.mapConsoleEvent consoleMessage console) servers

                Rhdm_authoring_ha name Nothing servers ->
                    Rhdm_authoring_ha name (Console.mapConsoleEvent consoleMessage Console.emptyConsole) servers

                Rhdm_production_immutable name (Just console) servers ->
                    Rhdm_production_immutable name (Console.mapConsoleEvent consoleMessage console) servers

                Rhdm_production_immutable name Nothing servers ->
                    Rhdm_production_immutable name (Console.mapConsoleEvent consoleMessage Console.emptyConsole) servers

                Rhpam_authoring_ha name (Just console) servers ->
                    Rhpam_authoring_ha name (Console.mapConsoleEvent consoleMessage console) servers

                Rhpam_authoring_ha name Nothing servers ->
                    Rhpam_authoring_ha name (Console.mapConsoleEvent consoleMessage Console.emptyConsole) servers

                Rhpam_authoring name (Just console) servers ->
                    Rhpam_authoring name (Console.mapConsoleEvent consoleMessage console) servers

                Rhpam_authoring name Nothing servers ->
                    Rhpam_authoring name (Console.mapConsoleEvent consoleMessage Console.emptyConsole) servers

                Rhpam_production_immutable name (Just console) servers ->
                    Rhpam_production_immutable name (Console.mapConsoleEvent consoleMessage console) servers

                Rhpam_production_immutable name Nothing servers ->
                    Rhpam_production_immutable name (Console.mapConsoleEvent consoleMessage Console.emptyConsole) servers

                Rhpam_production name (Just console) servers ->
                    Rhpam_production name (Console.mapConsoleEvent consoleMessage console) servers

                Rhpam_production name Nothing servers ->
                    Rhpam_production name (Console.mapConsoleEvent consoleMessage Console.emptyConsole) servers

                Rhpam_trial name (Just console) servers ->
                    Rhpam_trial name (Console.mapConsoleEvent consoleMessage console) servers

                Rhpam_trial name Nothing servers ->
                    Rhpam_trial name (Console.mapConsoleEvent consoleMessage Console.emptyConsole) servers

        ServersMsg serversMessage ->
            case environment of
                Rhdm_trial name console (Just servers) ->
                    Rhdm_trial name console (Servers.mapServersEvent serversMessage servers)

                Rhdm_trial name console Nothing ->
                    Rhdm_trial name console (Servers.mapServersEvent serversMessage { servers = [] })

                Rhdm_authoring name console (Just servers) ->
                    Rhdm_authoring name console (Servers.mapServersEvent serversMessage servers)

                Rhdm_authoring name console Nothing ->
                    Rhdm_authoring name console (Servers.mapServersEvent serversMessage { servers = [] })

                Rhdm_authoring_ha name console (Just servers) ->
                    Rhdm_authoring_ha name console (Servers.mapServersEvent serversMessage servers)

                Rhdm_authoring_ha name console Nothing ->
                    Rhdm_authoring_ha name console (Servers.mapServersEvent serversMessage { servers = [] })

                Rhdm_production_immutable name console (Just servers) ->
                    Rhdm_production_immutable name console (Servers.mapServersEvent serversMessage servers)

                Rhdm_production_immutable name console Nothing ->
                    Rhdm_production_immutable name console (Servers.mapServersEvent serversMessage { servers = [] })

                Rhpam_authoring_ha name console (Just servers) ->
                    Rhpam_authoring_ha name console (Servers.mapServersEvent serversMessage servers)

                Rhpam_authoring_ha name console Nothing ->
                    Rhpam_authoring_ha name console (Servers.mapServersEvent serversMessage { servers = [] })

                Rhpam_authoring name console (Just servers) ->
                    Rhpam_authoring name console (Servers.mapServersEvent serversMessage servers)

                Rhpam_authoring name console Nothing ->
                    Rhpam_authoring name console (Servers.mapServersEvent serversMessage { servers = [] })

                Rhpam_production_immutable name console (Just servers) ->
                    Rhpam_production_immutable name console (Servers.mapServersEvent serversMessage servers)

                Rhpam_production_immutable name console Nothing ->
                    Rhpam_production_immutable name console (Servers.mapServersEvent serversMessage { servers = [] })

                Rhpam_production name console (Just servers) ->
                    Rhpam_production name console (Servers.mapServersEvent serversMessage servers)

                Rhpam_production name console Nothing ->
                    Rhpam_production name console (Servers.mapServersEvent serversMessage { servers = [] })

                Rhpam_trial name console (Just servers) ->
                    Rhpam_trial name console (Servers.mapServersEvent serversMessage servers)

                Rhpam_trial name console Nothing ->
                    Rhpam_trial name console (Servers.mapServersEvent serversMessage { servers = [] })



-- VIEW


getEnvironmentView : (Msg -> msg) -> Environment -> List (Html msg)
getEnvironmentView msg environment =
    [ div [] [ text "Environment: ", select [ onInput (SelectEnvironment >> msg) ] (List.map (toEnvironmentOption environment) environments) ] ]
        ++ getConsoleView msg environment
        ++ getServersView msg environment


toEnvironmentOption : Environment -> Environment -> Html msg
toEnvironmentOption selectedEnvironment environment =
    option [ Html.Attributes.selected (getEnvironmentName selectedEnvironment == getEnvironmentName environment), value (getEnvironmentName environment) ] [ text (getEnvironmentName environment) ]


getConsoleView : (Msg -> msg) -> Environment -> List (Html msg)
getConsoleView msg environment =
    case environment of
        Rhdm_trial _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhdm_trial _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole

        Rhdm_authoring _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhdm_authoring _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole

        Rhdm_authoring_ha _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhdm_authoring_ha _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole

        Rhdm_production_immutable _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhdm_production_immutable _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole

        Rhpam_authoring_ha _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhpam_authoring_ha _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole

        Rhpam_authoring _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhpam_authoring _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole

        Rhpam_production_immutable _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhpam_production_immutable _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole

        Rhpam_production _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhpam_production _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole

        Rhpam_trial _ (Just console) _ ->
            Console.getConsoleView (ConsoleMsg >> msg) console

        Rhpam_trial _ Nothing _ ->
            Console.getConsoleView (ConsoleMsg >> msg) Console.emptyConsole


getServersView : (Msg -> msg) -> Environment -> List (Html msg)
getServersView msg environment =
    case environment of
        Rhdm_trial _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhdm_trial _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }

        Rhdm_authoring _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhdm_authoring _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }

        Rhdm_authoring_ha _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhdm_authoring_ha _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }

        Rhdm_production_immutable _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhdm_production_immutable _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }

        Rhpam_authoring_ha _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhpam_authoring_ha _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }

        Rhpam_authoring _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhpam_authoring _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }

        Rhpam_production_immutable _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhpam_production_immutable _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }

        Rhpam_production _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhpam_production _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }

        Rhpam_trial _ _ (Just servers) ->
            Servers.getServersView (ServersMsg >> msg) servers

        Rhpam_trial _ _ Nothing ->
            Servers.getServersView (ServersMsg >> msg) { servers = [] }



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

        Rhdm_authoring _ (Just console) _ ->
            Console.getConsoleAsYaml console intendation

        Rhdm_authoring_ha _ (Just console) _ ->
            Console.getConsoleAsYaml console intendation

        Rhdm_production_immutable _ (Just console) _ ->
            Console.getConsoleAsYaml console intendation

        Rhpam_authoring_ha _ (Just console) _ ->
            Console.getConsoleAsYaml console intendation

        Rhpam_authoring _ (Just console) _ ->
            Console.getConsoleAsYaml console intendation

        Rhpam_production_immutable _ (Just console) _ ->
            Console.getConsoleAsYaml console intendation

        Rhpam_production _ (Just console) _ ->
            Console.getConsoleAsYaml console intendation

        Rhpam_trial _ (Just console) _ ->
            Console.getConsoleAsYaml console intendation

        _ ->
            ""


getServersAsYaml : Environment -> Int -> String
getServersAsYaml environment intendation =
    case environment of
        Rhdm_trial _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        Rhdm_authoring _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        Rhdm_authoring_ha _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        Rhdm_production_immutable _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        Rhpam_authoring_ha _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        Rhpam_authoring _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        Rhpam_production_immutable _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        Rhpam_production _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        Rhpam_trial _ _ (Just servers) ->
            Servers.getServersAsYaml servers intendation

        _ ->
            ""
