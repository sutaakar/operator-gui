module Main exposing (KieApp, Msg(..), containsServer, getKieAppAsYaml, getObjectsAsYaml, getServerAsYaml, getServerView, init, main, update, view)

import Browser
import Environment
import Html exposing (Attribute, Html, div, input, option, select, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import ImageRegistry
import Server



-- MAIN


main : Program () KieApp Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias KieApp =
    { name : String
    , environment : Environment.Environment
    , server : Maybe Server.Server
    , imageRegistry : ImageRegistry.ImageRegistry
    }


init : KieApp
init =
    { name = "my-kie-app"
    , environment = Environment.rhdm_trial
    , server = Nothing
    , imageRegistry = ImageRegistry.emptyImageRegistry
    }


containsServer : KieApp -> Bool
containsServer kieApp =
    case kieApp.server of
        Just _ ->
            True

        Nothing ->
            False



-- UPDATE


type Msg
    = ChangeName String
    | SelectEnvironment String
    | ToggleServer
    | ServerMsg Server.Msg
    | ImageRegistryMsg ImageRegistry.Msg


update : Msg -> KieApp -> KieApp
update msg kieApp =
    case msg of
        ChangeName newName ->
            { kieApp | name = newName }

        SelectEnvironment newEnvironmentName ->
            { kieApp
                | environment =
                    case Environment.getEnvironmentFromName newEnvironmentName of
                        Nothing ->
                            Environment.rhdm_trial

                        Just newEnvironment ->
                            newEnvironment
            }

        ToggleServer ->
            { kieApp
                | server =
                    case kieApp.server of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just Server.emptyServer
            }

        ServerMsg serverMessage ->
            { kieApp
                | server =
                    case kieApp.server of
                        Just server ->
                            Just (Server.mapServerEvent serverMessage server)

                        Nothing ->
                            Nothing
            }

        ImageRegistryMsg imageRegistryMessage ->
            { kieApp | imageRegistry = ImageRegistry.mapImageRegistryEvent imageRegistryMessage kieApp.imageRegistry }



-- VIEW


view : KieApp -> Html Msg
view kieApp =
    div []
        ([ div [] [ text "Kie app name: ", input [ placeholder "Kie app name", value kieApp.name, onInput ChangeName ] [] ]
         , div [] [ text "Environment: ", Environment.getEnvironmentDropdownList kieApp.environment SelectEnvironment ]
         ]
            ++ [ div [] [ input [ type_ "checkbox", checked (containsServer kieApp), onClick ToggleServer ] [], text "Kie server common config" ] ]
            ++ getServerView kieApp
            ++ ImageRegistry.getImageRegistryView kieApp.imageRegistry ImageRegistryMsg
            ++ [ div [] [ textarea [ cols 80, rows 25, readonly True ] [ text (getKieAppAsYaml kieApp) ] ] ]
        )


getServerView : KieApp -> List (Html Msg)
getServerView kieApp =
    case kieApp.server of
        Just server ->
            Server.getServerView server ServerMsg

        Nothing ->
            []


getKieAppAsYaml : KieApp -> String
getKieAppAsYaml kieApp =
    "apiVersion: app.kiegroup.org/v1\n"
        ++ "kind: KieApp\n"
        ++ "metadata:\n"
        ++ "  name: "
        ++ kieApp.name
        ++ "\n"
        ++ "spec:\n"
        ++ "  environment: "
        ++ Environment.getEnvironmentName kieApp.environment
        ++ "\n"
        ++ getObjectsAsYaml kieApp
        ++ getServerAsYaml kieApp
        ++ ImageRegistry.getImageRegistryAsYaml kieApp.imageRegistry


getObjectsAsYaml : KieApp -> String
getObjectsAsYaml kieApp =
    if containsServer kieApp then
        "  objects:" ++ "\n"

    else
        ""


getServerAsYaml : KieApp -> String
getServerAsYaml kieApp =
    case kieApp.server of
        Just server ->
            Server.getServerAsYaml server

        Nothing ->
            ""
