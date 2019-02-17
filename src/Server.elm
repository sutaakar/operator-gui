module Server exposing (Server, Msg, emptyServer, getServerView, mapServerEvent, getServerAsYaml)

import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MODEL

type Env_value
  = Value String
  | ValueFrom String

type alias Env_item =
  { name : String
  , value : Env_value}

type alias Spec =
  { env : List Env_item }

type alias Server =
  { deployments : Maybe Int
  , spec : Spec
  }

emptyServer : Server
emptyServer =
  {deployments = Nothing
  , spec = {env = []}}

getDeploymentsAsString : Server -> String
getDeploymentsAsString server =
  case server.deployments of
    Just deployments ->
      String.fromInt deployments
    Nothing ->
      ""

setEnvName : String -> Env_item -> Env_item
setEnvName newName envItem =
  {envItem | name = newName}

updateSingleEnvItemWithIndex : Int -> (Env_item -> Env_item) -> Int -> Env_item -> Env_item
updateSingleEnvItemWithIndex updateIndex envItemUpdate envIndex env =
  if updateIndex == envIndex then
    envItemUpdate env
  else
    env

updateEnvItemInList : Int -> (Env_item -> Env_item) -> List Env_item -> List Env_item
updateEnvItemInList updateIndex envItemUpdate envItems =
  if List.length envItems > updateIndex then
    List.indexedMap (updateSingleEnvItemWithIndex updateIndex envItemUpdate) envItems |>
    List.filter (\envItem -> not (String.isEmpty envItem.name))
  else
    envItems ++ [envItemUpdate { name = "", value = Value "" }]


updateEnvItemInSpec : Int -> (Env_item -> Env_item) -> Spec -> Spec
updateEnvItemInSpec updateIndex envItemUpdate spec =
  {spec | env = updateEnvItemInList updateIndex envItemUpdate spec.env}

-- UPDATE

type Msg
  = ChangeDeployments String
  | ChangeEnvVariableName Int String

mapServerEvent : Msg -> Server -> Server
mapServerEvent msg server =
  case msg of
    ChangeDeployments depl ->
      { server | deployments = String.toInt depl }
    ChangeEnvVariableName updateIndex newName ->
      { server | spec = updateEnvItemInSpec updateIndex (setEnvName newName) server.spec}


-- VIEW

getServerView : Server -> (Msg -> msg) -> List (Html msg)
getServerView server msg =
  [ div [] [ text "Number of Kie server deployments: ", input [ placeholder "Deployments", value (getDeploymentsAsString server), onInput (ChangeDeployments >> msg) ] [] ] ]
  ++ getEnvVariablesView server msg

getEnvVariablesView : Server -> (Msg -> msg) -> List (Html msg)
getEnvVariablesView server msg =
  List.indexedMap (getSingleEnvVariableView msg) server.spec.env
  ++ [getSingleEnvVariableView msg (List.length server.spec.env) { name = "", value = Value "" }]

getSingleEnvVariableView : (Msg -> msg) -> Int -> Env_item -> Html msg
getSingleEnvVariableView msg index env_item =
  div [] [ text "Env variable name: ", input [ placeholder "Name", value env_item.name, onInput (ChangeEnvVariableName index >> msg) ] [] ]


getServerAsYaml : Server -> String
getServerAsYaml server =
  "    server:" ++ "\n"
  ++ getDeploymentsAsYaml server
  ++ getSpecAsYaml server
  ++ getEnvItemsAsYaml server

getDeploymentsAsYaml : Server -> String
getDeploymentsAsYaml server =
  case server.deployments of
      Just int ->
        "      deployments: " ++ String.fromInt int  ++ "\n"
      Nothing ->
        ""

getSpecAsYaml : Server -> String
getSpecAsYaml server =
  if List.length server.spec.env > 0 then
      "      spec:"  ++ "\n"
  else
    ""

getEnvItemsAsYaml : Server -> String
getEnvItemsAsYaml server =
  if List.length server.spec.env > 0 then
    "        env:"  ++ "\n"
    ++ (List.map getEnvVariableAsYaml server.spec.env |>
        List.foldr (++) "")
  else
    ""

getEnvVariableAsYaml : Env_item -> String
getEnvVariableAsYaml envItem =
  "          - name: " ++ envItem.name  ++ "\n"
  ++ case envItem.value of
    Value value ->
      "            value: \"" ++ value ++ "\""  ++ "\n"
    ValueFrom value ->
      "            valueFrom: \"" ++ value ++ "\""  ++ "\n"
