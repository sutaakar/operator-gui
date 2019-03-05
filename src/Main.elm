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
    , imageRegistry : Maybe ImageRegistry.ImageRegistry
    }


init : KieApp
init =
    { name = "my-kie-app"
    , environment = Environment.rhdm_trial
    , server = Nothing
    , imageRegistry = Nothing
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
    | EnvironmentMsg Environment.Msg
    | ToggleServer
    | ServerMsg Server.Msg
    | ImageRegistryMsg ImageRegistryMsg


type ImageRegistryMsg
    = ToggleInsecure ImageRegistry.ImageRegistry
    | ChangeRegistryName ImageRegistry.ImageRegistry String


update : Msg -> KieApp -> KieApp
update msg kieApp =
    case msg of
        ChangeName newName ->
            { kieApp | name = newName }

        EnvironmentMsg environmentMessage ->
            { kieApp
                | environment = updateEnvironment environmentMessage kieApp.environment
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

        ImageRegistryMsg imageRegistryMsg ->
            { kieApp | imageRegistry = updateImageRegistry imageRegistryMsg }


updateEnvironment : Environment.Msg -> Environment.Environment -> Environment.Environment
updateEnvironment environmentMsg environment =
    Environment.mapEnvironmentEvent environmentMsg environment


updateImageRegistry : ImageRegistryMsg -> Maybe ImageRegistry.ImageRegistry
updateImageRegistry imageRegistryMsg =
    case imageRegistryMsg of
        ToggleInsecure imageReg ->
            Just { imageReg | insecure = not imageReg.insecure }

        ChangeRegistryName imageReg newRegistryName ->
            if String.length newRegistryName > 0 then
                Just { imageReg | registry = newRegistryName }

            else
                Nothing



-- VIEW


view : KieApp -> Html Msg
view kieApp =
    div []
        ([ div [] [ text "Kie app name: ", input [ placeholder "Kie app name", value kieApp.name, onInput ChangeName ] [] ]
         ]
            ++ Environment.getEnvironmentView EnvironmentMsg kieApp.environment
            ++ [ div [] [ input [ type_ "checkbox", checked (containsServer kieApp), onClick ToggleServer ] [], text "Kie server common config" ] ]
            ++ getServerView kieApp
            ++ getImageRegistryView kieApp
            ++ [ div [] [ textarea [ cols 80, rows 25, readonly True ] [ text (getKieAppAsYaml kieApp) ] ] ]
        )


getServerView : KieApp -> List (Html Msg)
getServerView kieApp =
    case kieApp.server of
        Just server ->
            Server.getServerView server ServerMsg

        Nothing ->
            []


getImageRegistryView : KieApp -> List (Html Msg)
getImageRegistryView kieApp =
    let
        insecureInput : Html Msg
        insecureInput =
            case kieApp.imageRegistry of
                Just imageReg ->
                    input [ type_ "checkbox", checked imageReg.insecure, onClick (ImageRegistryMsg (ToggleInsecure imageReg)) ] []

                Nothing ->
                    input [ type_ "checkbox", disabled True, checked False ] []

        changeRegistryInput : Html Msg
        changeRegistryInput =
            case kieApp.imageRegistry of
                Just imageReg ->
                    input [ placeholder "Registry", value imageReg.registry, onInput (ChangeRegistryName imageReg >> ImageRegistryMsg) ] []

                Nothing ->
                    input [ placeholder "Registry", onInput (ChangeRegistryName ImageRegistry.emptyImageRegistry >> ImageRegistryMsg) ] []
    in
    [ Html.form []
        [ Html.fieldset []
            [ Html.legend [] [ text "Image registry configuration" ]
            , insecureInput
            , text "Insecure registry"
            , Html.br [] []
            , text "Registry for Kie images: "
            , changeRegistryInput
            ]
        ]
    ]


getKieAppAsYaml : KieApp -> String
getKieAppAsYaml kieApp =
    "apiVersion: app.kiegroup.org/v1\n"
        ++ "kind: KieApp\n"
        ++ "metadata:\n"
        ++ "  name: "
        ++ kieApp.name
        ++ "\n"
        ++ "spec:\n"
        ++ Environment.getEnvironmentAsYaml kieApp.environment
        ++ getObjectsAsYaml kieApp
        ++ getServerAsYaml kieApp
        ++ getImageRegistryAsYaml kieApp


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


getImageRegistryAsYaml : KieApp -> String
getImageRegistryAsYaml kieApp =
    case kieApp.imageRegistry of
        Just imageRegistry ->
            ImageRegistry.getImageRegistryAsYaml imageRegistry

        Nothing ->
            ""
