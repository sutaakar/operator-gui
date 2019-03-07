module Server exposing (Msg, Server, emptyServer, getServerAsYaml, getServerView, mapServerEvent)

import Html exposing (Attribute, Html, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MODEL


type Env_value
    = Value String
    | ValueFrom String


type alias Env_item =
    { name : String
    , value : Env_value
    }


type From
    = ImageStreamTag String String
    | DockerImage String


type alias Spec =
    { env : List Env_item }


type alias Server =
    { deployments : Maybe Int
    , from : Maybe From
    , spec : Spec
    }


emptyServer : Server
emptyServer =
    { deployments = Nothing
    , from = Nothing
    , spec = { env = [] }
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


setEnvName : String -> Env_item -> Env_item
setEnvName newName envItem =
    { envItem | name = newName }


getEnvValueAsString : Env_value -> String
getEnvValueAsString envValue =
    case envValue of
        Value value ->
            value

        ValueFrom valueFrom ->
            valueFrom


setEnvValue : String -> Env_item -> Env_item
setEnvValue newValue envItem =
    { envItem | value = Value newValue }


updateSingleEnvItemWithIndex : Int -> (Env_item -> Env_item) -> Int -> Env_item -> Env_item
updateSingleEnvItemWithIndex updateIndex envItemUpdate envIndex env =
    if updateIndex == envIndex then
        envItemUpdate env

    else
        env


updateEnvItemInList : Int -> (Env_item -> Env_item) -> List Env_item -> List Env_item
updateEnvItemInList updateIndex envItemUpdate envItems =
    if List.length envItems > updateIndex then
        List.indexedMap (updateSingleEnvItemWithIndex updateIndex envItemUpdate) envItems
            |> List.filter (\envItem -> not (String.isEmpty envItem.name))

    else
        envItems ++ [ envItemUpdate { name = "", value = Value "" } ]


updateEnvItemInSpec : Int -> (Env_item -> Env_item) -> Spec -> Spec
updateEnvItemInSpec updateIndex envItemUpdate spec =
    { spec | env = updateEnvItemInList updateIndex envItemUpdate spec.env }



-- UPDATE


type Msg
    = ChangeDeployments String
    | ChangeFrom String
    | ChangeFromName String
    | ChangeFromNamespace String
    | ChangeEnvVariableName Int String
    | ChangeEnvVariableValue Int String


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

        ChangeEnvVariableName updateIndex newName ->
            { server | spec = updateEnvItemInSpec updateIndex (setEnvName newName) server.spec }

        ChangeEnvVariableValue updateIndex newValue ->
            { server | spec = updateEnvItemInSpec updateIndex (setEnvValue newValue) server.spec }



-- VIEW


getServerView : (Msg -> msg) -> Server -> List (Html msg)
getServerView msg server =
    [ div [] [ text "Number of Kie server deployments: ", input [ placeholder "Deployments", value (getDeploymentsAsString server), onInput (ChangeDeployments >> msg) ] [] ] ]
        ++ getFromView server.from msg
        ++ getEnvVariablesView server msg


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


getEnvVariablesView : Server -> (Msg -> msg) -> List (Html msg)
getEnvVariablesView server msg =
    List.indexedMap (getSingleEnvVariableView msg False) server.spec.env
        ++ [ getSingleEnvVariableView msg True (List.length server.spec.env) { name = "", value = Value "" } ]


getSingleEnvVariableView : (Msg -> msg) -> Bool -> Int -> Env_item -> Html msg
getSingleEnvVariableView msg lastEntryLine index env_item =
    div []
        [ text "Env variable name: "
        , input [ placeholder "Name", value env_item.name, onInput (ChangeEnvVariableName index >> msg) ] []
        , text "Env variable value: "
        , input [ placeholder "Value", readonly lastEntryLine, value (getEnvValueAsString env_item.value), onInput (ChangeEnvVariableValue index >> msg) ] []
        ]


getServerAsYaml : Server -> String
getServerAsYaml server =
    "    server:"
        ++ "\n"
        ++ getDeploymentsAsYaml server
        ++ getFromAsYaml server
        ++ getSpecAsYaml server
        ++ getEnvItemsAsYaml server


getDeploymentsAsYaml : Server -> String
getDeploymentsAsYaml server =
    case server.deployments of
        Just int ->
            "      deployments: " ++ String.fromInt int ++ "\n"

        Nothing ->
            ""


getFromAsYaml : Server -> String
getFromAsYaml server =
    case server.from of
        Just from ->
            case from of
                ImageStreamTag name namespace ->
                    "      from:\n"
                        ++ "        kind: ImageStreamTag\n"
                        ++ "        name: "
                        ++ name
                        ++ "\n"
                        ++ (if String.length namespace > 0 then
                                "        namespace: " ++ namespace ++ "\n"

                            else
                                ""
                           )

                DockerImage name ->
                    "      from:\n"
                        ++ "        kind: DockerImage\n"
                        ++ "        name: "
                        ++ name
                        ++ "\n"

        Nothing ->
            ""


getSpecAsYaml : Server -> String
getSpecAsYaml server =
    if List.length server.spec.env > 0 then
        "      spec:" ++ "\n"

    else
        ""


getEnvItemsAsYaml : Server -> String
getEnvItemsAsYaml server =
    if List.length server.spec.env > 0 then
        "        env:"
            ++ "\n"
            ++ (List.map getEnvVariableAsYaml server.spec.env
                    |> List.foldr (++) ""
               )

    else
        ""


getEnvVariableAsYaml : Env_item -> String
getEnvVariableAsYaml envItem =
    "          - name: "
        ++ envItem.name
        ++ "\n"
        ++ (case envItem.value of
                Value value ->
                    "            value: \"" ++ value ++ "\"" ++ "\n"

                ValueFrom value ->
                    "            valueFrom: \"" ++ value ++ "\"" ++ "\n"
           )
