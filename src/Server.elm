module Server exposing (Msg, Server, emptyServer, getServerAsYaml, getServerView, mapServerEvent)

import EnvItem
import Html exposing (Attribute, Html, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import YamlUtils



-- MODEL


type From
    = ImageStreamTag String String
    | DockerImage String


type alias Server =
    { deployments : Maybe Int
    , from : Maybe From
    , env : Maybe (List EnvItem.EnvItem)
    }


emptyServer : Server
emptyServer =
    { deployments = Nothing
    , from = Nothing
    , env = Nothing
    }


getDeploymentsAsString : Server -> String
getDeploymentsAsString server =
    case server.deployments of
        Just deployments ->
            String.fromInt deployments

        Nothing ->
            ""


getFromFromName : String -> Maybe From
getFromFromName fromName =
    if fromName == "ImageStreamTag" then
        Just (ImageStreamTag "" "")

    else if fromName == "DockerImage" then
        Just (DockerImage "")

    else
        Nothing



-- UPDATE


type Msg
    = ChangeDeployments String
    | ChangeFrom String
    | ChangeFromName String
    | ChangeFromNamespace String
    | EnvItemMsg EnvItem.Msg


mapServerEvent : Msg -> Server -> Server
mapServerEvent msg server =
    case msg of
        ChangeDeployments depl ->
            { server | deployments = String.toInt depl }

        ChangeFrom fromName ->
            { server | from = getFromFromName fromName }

        ChangeFromName newName ->
            case server.from of
                Just (ImageStreamTag _ namespace) ->
                    { server | from = Just (ImageStreamTag newName namespace) }

                Just (DockerImage _) ->
                    { server | from = Just (DockerImage newName) }

                _ ->
                    server

        ChangeFromNamespace newNamespace ->
            case server.from of
                Just (ImageStreamTag name namespace) ->
                    { server | from = Just (ImageStreamTag name newNamespace) }

                _ ->
                    server

        EnvItemMsg envItemMessage ->
            case server.env of
                Just envItems ->
                    { server | env = EnvItem.mapEnvItemEvent envItemMessage envItems }

                Nothing ->
                    { server | env = EnvItem.mapEnvItemEvent envItemMessage [] }



-- VIEW


getServerView : (Msg -> msg) -> Server -> List (Html msg)
getServerView msg server =
    [ div [] [ text "Number of Kie server deployments: ", input [ placeholder "Deployments", value (getDeploymentsAsString server), onInput (ChangeDeployments >> msg) ] [] ] ]
        ++ getFromView server.from msg
        ++ getEnvVariableView msg server


getFromView : Maybe From -> (Msg -> msg) -> List (Html msg)
getFromView selectedFrom msg =
    case selectedFrom of
        Nothing ->
            [ select [ onInput (ChangeFrom >> msg) ] (getFromOptions selectedFrom) ]

        Just (ImageStreamTag name namespace) ->
            [ select [ onInput (ChangeFrom >> msg) ] (getFromOptions selectedFrom)
            , text "Name: "
            , input [ placeholder "Name", value name, onInput (ChangeFromName >> msg) ] []
            , text "Namespace: "
            , input [ placeholder "Keep empty for default", value namespace, onInput (ChangeFromNamespace >> msg) ] []
            ]

        Just (DockerImage name) ->
            [ select [ onInput (ChangeFrom >> msg) ] (getFromOptions selectedFrom)
            , text "Name: "
            , input [ placeholder "Name", value name, onInput (ChangeFromName >> msg) ] []
            ]


getFromOptions : Maybe From -> List (Html msg)
getFromOptions selectedFrom =
    case selectedFrom of
        Nothing ->
            [ option [ Html.Attributes.selected True, value "" ] [ text "" ]
            , option [ Html.Attributes.selected False, value "ImageStreamTag" ] [ text "ImageStreamTag" ]
            , option [ Html.Attributes.selected False, value "DockerImage" ] [ text "DockerImage" ]
            ]

        Just (ImageStreamTag _ _) ->
            [ option [ Html.Attributes.selected False, value "" ] [ text "" ]
            , option [ Html.Attributes.selected True, value "ImageStreamTag" ] [ text "ImageStreamTag" ]
            , option [ Html.Attributes.selected False, value "DockerImage" ] [ text "DockerImage" ]
            ]

        Just (DockerImage _) ->
            [ option [ Html.Attributes.selected False, value "" ] [ text "" ]
            , option [ Html.Attributes.selected False, value "ImageStreamTag" ] [ text "ImageStreamTag" ]
            , option [ Html.Attributes.selected True, value "DockerImage" ] [ text "DockerImage" ]
            ]


getEnvVariableView : (Msg -> msg) -> Server -> List (Html msg)
getEnvVariableView msg server =
    case server.env of
        Just envItems ->
            EnvItem.getEnvVariableView (EnvItemMsg >> msg) envItems

        Nothing ->
            [ EnvItem.getLastEnvVariableView (EnvItemMsg >> msg) ]



-- YAML


getServerAsYaml : Server -> Int -> String
getServerAsYaml server intendation =
    getDeploymentsAsYaml server (intendation + 1)
        ++ getFromAsYaml server (intendation + 1)
        ++ getEnvAsYaml server (intendation + 1)
        |> String.dropLeft ((intendation + 1) * 2)
        |> String.append (String.repeat intendation "  " ++ "- ")


getDeploymentsAsYaml : Server -> Int -> String
getDeploymentsAsYaml server intendation =
    case server.deployments of
        Just int ->
            YamlUtils.getNameAndValueWithIntendation "deployments" (String.fromInt int) intendation

        Nothing ->
            ""


getFromAsYaml : Server -> Int -> String
getFromAsYaml server intendation =
    case server.from of
        Just (ImageStreamTag name namespace) ->
            YamlUtils.getNameWithIntendation "from" intendation
                ++ YamlUtils.getNameAndValueWithIntendation "kind" "ImageStreamTag" (intendation + 1)
                ++ YamlUtils.getNameAndValueWithIntendation "name" name (intendation + 1)
                ++ (if String.length namespace > 0 then
                        YamlUtils.getNameAndValueWithIntendation "namespace" namespace (intendation + 1)

                    else
                        ""
                   )

        Just (DockerImage name) ->
            YamlUtils.getNameWithIntendation "from" intendation
                ++ YamlUtils.getNameAndValueWithIntendation "kind" "DockerImage" (intendation + 1)
                ++ YamlUtils.getNameAndValueWithIntendation "name" name (intendation + 1)

        Nothing ->
            ""


getEnvAsYaml : Server -> Int -> String
getEnvAsYaml server intendation =
    case server.env of
        Just envItems ->
            EnvItem.getEnvAsYaml envItems intendation

        Nothing ->
            ""
