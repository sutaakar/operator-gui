import Browser
import Html exposing (Html, Attribute, div, text, textarea, input, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Environment
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
  }

init : KieApp
init =
  { name = "trial"
  , environment = Environment.trial
  , server = Nothing }

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

update : Msg -> KieApp -> KieApp
update msg kieApp =
  case msg of
    ChangeName newName ->
      { kieApp | name = newName }
    SelectEnvironment newEnvironmentName ->
      { kieApp | environment =
        case Environment.getEnvironmentFromName newEnvironmentName of
          Nothing ->
            Environment.trial
          Just newEnvironment ->
            newEnvironment
      }
    ToggleServer ->
      { kieApp | server =
        case kieApp.server of
          Just _ ->
            Nothing
          Nothing ->
            Just Server.emptyServer
      }
    ServerMsg serverMessage ->
      { kieApp | server =
        case kieApp.server of
          Just server ->
            Just (Server.mapServerEvent serverMessage server)
          Nothing ->
            Nothing
      }



-- VIEW

view : KieApp -> Html Msg
view kieApp =
  div []
    (
      [ div [] [ text "Kie app name: ", input [ placeholder "Kie app name", value kieApp.name, onInput ChangeName ] [] ]
      , div [] [ text "Environment: ", Environment.getEnvironmentDropdownList kieApp.environment SelectEnvironment ]
      ]
      ++ [ div [] [ input [ type_ "checkbox", checked (containsServer kieApp), onClick ToggleServer ] [], text "Servers" ] ]
      ++ getServerView kieApp
      ++ [ div [] [ textarea [ cols 40, rows 10, readonly True ] [ text (getKieAppAsYaml kieApp) ] ] ]
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
  ++ "  name: " ++ kieApp.name ++ "\n"
  ++ "spec:\n"
  ++ "  environment: " ++ Environment.getEnvironmentName kieApp.environment ++ "\n"
  ++ getObjectsAsYaml kieApp
  ++ getServerAsYaml kieApp


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
