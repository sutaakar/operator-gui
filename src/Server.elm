module Server exposing (Server, Msg, emptyServer, getServerView, mapServerEvent, getServerAsYaml)

import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MODEL

type alias Server =
  { deployments : Maybe Int
  }

emptyServer : Server
emptyServer =
  {deployments = Nothing}

getDeploymentsAsString : Server -> String
getDeploymentsAsString server =
  case server.deployments of
    Just deployments ->
      String.fromInt deployments
    Nothing ->
      ""

-- UPDATE

type Msg
  = ChangeDeployments String

mapServerEvent : Msg -> Server -> Server
mapServerEvent msg server =
  case msg of
    ChangeDeployments depl ->
      { server | deployments = String.toInt depl }


-- VIEW

getServerView : Server -> (Msg -> msg) -> List (Html msg)
getServerView server msg =
  [ div [] [ text "Number of Kie server deployments: ", input [ placeholder "Deployments", value (getDeploymentsAsString server), onInput (ChangeDeployments >> msg) ] [] ] ]

getServerAsYaml : Server -> String
getServerAsYaml server =
  "    server:" ++ "\n"
  ++ getDeploymentsAsYaml server

getDeploymentsAsYaml : Server -> String
getDeploymentsAsYaml server =
  case server.deployments of
      Just int ->
        "      deployments: " ++ String.fromInt int  ++ "\n"
      Nothing ->
        ""
