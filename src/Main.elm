module Main exposing (KieApp, Msg(..), init, main, update, view)

import Browser
import Environment
import Html exposing (Attribute, Html, div, input, option, select, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import ImageRegistry
import YamlUtils



-- MAIN


main : Program () KieApp Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias KieApp =
    { name : String
    , environment : Environment.Environment
    , imageRegistry : Maybe ImageRegistry.ImageRegistry
    }


init : KieApp
init =
    { name = "my-kie-app"
    , environment = Environment.rhdm_trial
    , imageRegistry = Nothing
    }



-- UPDATE


type Msg
    = ChangeName String
    | EnvironmentMsg Environment.Msg
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
            ++ getImageRegistryView kieApp
            ++ [ div [] [ textarea [ cols 80, rows 25, readonly True ] [ text (getKieAppAsYaml kieApp) ] ] ]
        )


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
    YamlUtils.getNameAndValueWithIntendation "apiVersion" "app.kiegroup.org/v1" 0
        ++ YamlUtils.getNameAndValueWithIntendation "kind" "KieApp" 0
        ++ YamlUtils.getNameWithIntendation "metadata" 0
        ++ YamlUtils.getNameAndValueWithIntendation "name" kieApp.name 1
        ++ YamlUtils.getNameWithIntendation "spec" 0
        ++ Environment.getEnvironmentAsYaml kieApp.environment 1
        ++ getImageRegistryAsYaml kieApp 1


getImageRegistryAsYaml : KieApp -> Int -> String
getImageRegistryAsYaml kieApp intendation =
    case kieApp.imageRegistry of
        Just imageRegistry ->
            ImageRegistry.getImageRegistryAsYaml imageRegistry intendation

        Nothing ->
            ""
