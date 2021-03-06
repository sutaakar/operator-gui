module Servers exposing (Msg, Servers, getServersAsYaml, getServersView, mapServersEvent)

import Html exposing (Attribute, Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Server
import YamlUtils



-- MODEL


type alias Servers =
    { servers : List Server.Server
    }



-- UPDATE


type Msg
    = ServerMsg Server.Server Server.Msg


mapServersEvent : Msg -> Servers -> Maybe Servers
mapServersEvent msg servers =
    case msg of
        ServerMsg updatedServer serverMessage ->
            case
                addServerIfNotExists updatedServer servers.servers
                    |> mapServerEventInServers updatedServer serverMessage
                    |> List.filter (\server -> not (Server.emptyServer == server))
            of
                [] ->
                    Nothing

                xs ->
                    Just { servers | servers = xs }


addServerIfNotExists : Server.Server -> List Server.Server -> List Server.Server
addServerIfNotExists updatedServer servers =
    if List.member updatedServer servers then
        servers

    else
        servers ++ [ updatedServer ]


mapServerEventInServers : Server.Server -> Server.Msg -> List Server.Server -> List Server.Server
mapServerEventInServers updatedServer serverMessage servers =
    List.map
        (\server ->
            if server == updatedServer then
                Server.mapServerEvent serverMessage server

            else
                server
        )
        servers



-- VIEW


getServersView : (Msg -> msg) -> Servers -> Bool -> List (Html msg)
getServersView msg servers withDatabase =
    List.concatMap (\server -> Server.getServerView (ServerMsg server >> msg) server withDatabase) servers.servers
        ++ Server.getServerView (ServerMsg Server.emptyServer >> msg) Server.emptyServer withDatabase



-- YAML


getServersAsYaml : Servers -> Int -> String
getServersAsYaml servers intendation =
    YamlUtils.getNameWithIntendation "servers" intendation
        ++ (List.map (\server -> Server.getServerAsYaml server (intendation + 1)) servers.servers
                |> List.foldr (++) ""
           )
