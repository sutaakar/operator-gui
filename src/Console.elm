module Console exposing (Console, Msg, emptyConsole, getConsoleAsYaml, getConsoleView, getConsoleViewEmpty, mapConsoleEvent)

import Html exposing (Attribute, Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MODEL


type alias EnvItem =
    { name : String
    , value : String
    }


type alias SsoClient =
    { name : String
    , secret : String
    , hostnameHttp : String
    , hostnameHttps : String
    }


type alias Console =
    { env : Maybe (List EnvItem)
    , ssoClient : Maybe SsoClient
    }


emptyConsole : Console
emptyConsole =
    { env = Nothing
    , ssoClient = Nothing
    }



-- UPDATE


type Msg
    = AddNewEnvVariable String
    | UpdateExistingEnvVariableName EnvItem (List EnvItem) String
    | UpdateExistingEnvVariableValue EnvItem (List EnvItem) String
    | AddNewSssoClientName String
    | UpdateExistingSsoClientName SsoClient String
    | UpdateExistingSsoSecret SsoClient String
    | UpdateExistingSsoHostnameHttp SsoClient String
    | UpdateExistingSsoHostnameHttps SsoClient String


mapConsoleEvent : Msg -> Console -> Maybe Console
mapConsoleEvent msg console =
    case msg of
        AddNewEnvVariable newEnvVariableName ->
            case console.env of
                Just envItemList ->
                    Just { console | env = Just (envItemList ++ [ { name = newEnvVariableName, value = "" } ]) }

                Nothing ->
                    Just { console | env = Just [ { name = newEnvVariableName, value = "" } ] }

        UpdateExistingEnvVariableName updatedEnvVariable envVariables newEnvVariableName ->
            case updateEnvItemInList updatedEnvVariable (\envItem -> { envItem | name = newEnvVariableName }) envVariables of
                [] ->
                    { console | env = Nothing } |> checkConsoleContent

                xs ->
                    Just { console | env = Just xs }

        UpdateExistingEnvVariableValue updatedEnvVariable envVariables newEnvVariableValue ->
            Just { console | env = Just (updateEnvItemInList updatedEnvVariable (\envItem -> { envItem | value = newEnvVariableValue }) envVariables) }

        AddNewSssoClientName newSsoClientName ->
            Just { console | ssoClient = Just { name = newSsoClientName, secret = "", hostnameHttp = "", hostnameHttps = "" } }

        UpdateExistingSsoClientName updatedSsoClient updatedSsoClientName ->
            if String.length updatedSsoClientName == 0 then
                { console | ssoClient = Nothing } |> checkConsoleContent

            else
                Just { console | ssoClient = Just { updatedSsoClient | name = updatedSsoClientName } }

        UpdateExistingSsoSecret updatedSsoClient updatedSsoSecret ->
            Just { console | ssoClient = Just { updatedSsoClient | secret = updatedSsoSecret } }

        UpdateExistingSsoHostnameHttp updatedSsoClient updatedSsoHostnameHttp ->
            Just { console | ssoClient = Just { updatedSsoClient | hostnameHttp = updatedSsoHostnameHttp } }

        UpdateExistingSsoHostnameHttps updatedSsoClient updatedSsoHostnameHttps ->
            Just { console | ssoClient = Just { updatedSsoClient | hostnameHttps = updatedSsoHostnameHttps } }


updateEnvItemInList : EnvItem -> (EnvItem -> EnvItem) -> List EnvItem -> List EnvItem
updateEnvItemInList updatedItem envItemUpdateFunction envItems =
    List.map (updateSingleEnvItem envItemUpdateFunction updatedItem) envItems
        |> List.filter (\envItem -> not (String.isEmpty envItem.name))


updateSingleEnvItem : (EnvItem -> EnvItem) -> EnvItem -> EnvItem -> EnvItem
updateSingleEnvItem envItemUpdateFunction updatedItem itemFromList =
    if updatedItem == itemFromList then
        envItemUpdateFunction itemFromList

    else
        itemFromList


checkConsoleContent : Console -> Maybe Console
checkConsoleContent console =
    let
        checkEnvVariablesExistence : Bool
        checkEnvVariablesExistence =
            case console.env of
                Just envItemList ->
                    True

                Nothing ->
                    False

        checkSsoClientExistence : Bool
        checkSsoClientExistence =
            case console.ssoClient of
                Just ssoClient ->
                    True

                Nothing ->
                    False
    in
    if checkEnvVariablesExistence || checkSsoClientExistence then
        Just console

    else
        Nothing



-- VIEW


getConsoleViewEmpty : (Msg -> msg) -> List (Html msg)
getConsoleViewEmpty msg =
    getConsoleView msg emptyConsole


getConsoleView : (Msg -> msg) -> Console -> List (Html msg)
getConsoleView msg console =
    [ Html.fieldset []
        ([ Html.legend [] [ text "Monitoring console configuration" ] ]
            ++ getEnvVariableView msg console
            ++ getSsoClientView msg console
        )
    ]


getEnvVariableView : (Msg -> msg) -> Console -> List (Html msg)
getEnvVariableView msg console =
    case console.env of
        Just envItemList ->
            List.map (getSingleEnvVariableView msg envItemList) envItemList ++ [ getLastEnvVariableView msg ]

        Nothing ->
            [ getLastEnvVariableView msg ]


getSingleEnvVariableView : (Msg -> msg) -> List EnvItem -> EnvItem -> Html msg
getSingleEnvVariableView msg envItemList envItem =
    div []
        [ text "Env variable name: "
        , input [ placeholder "Name", value envItem.name, onInput (UpdateExistingEnvVariableName envItem envItemList >> msg) ] []
        , text "Env variable value: "
        , input [ placeholder "Value", value envItem.value, onInput (UpdateExistingEnvVariableValue envItem envItemList >> msg) ] []
        ]


getLastEnvVariableView : (Msg -> msg) -> Html msg
getLastEnvVariableView msg =
    div []
        [ text "Env variable name: "
        , input [ placeholder "Name", onInput (AddNewEnvVariable >> msg) ] []
        ]


getSsoClientView : (Msg -> msg) -> Console -> List (Html msg)
getSsoClientView msg console =
    case console.ssoClient of
        Just ssoClient ->
            [ text "SSO client name: "
            , input [ placeholder "Name", value ssoClient.name, onInput (UpdateExistingSsoClientName ssoClient >> msg) ] []
            , text "SSO secret: "
            , input [ placeholder "Secret", value ssoClient.secret, onInput (UpdateExistingSsoSecret ssoClient >> msg) ] []
            , br [] []
            , text "HTTP redirect URL hostname: "
            , input [ placeholder "Hostname redirect URL", value ssoClient.hostnameHttp, onInput (UpdateExistingSsoHostnameHttp ssoClient >> msg) ] []
            , text "HTTPS redirect URL hostname: "
            , input [ placeholder "Secure hostname redirect URL", value ssoClient.hostnameHttps, onInput (UpdateExistingSsoHostnameHttps ssoClient >> msg) ] []
            ]

        Nothing ->
            [ text "SSO client name: "
            , input [ placeholder "Name", onInput (AddNewSssoClientName >> msg) ] []
            ]



-- YAML


getConsoleAsYaml : Console -> String
getConsoleAsYaml console =
    "    console:\n"
        ++ getEnvAsYaml console
        ++ getSsoClientAsYaml console


getEnvAsYaml : Console -> String
getEnvAsYaml console =
    case console.env of
        Just envItemList ->
            "      env:\n"
                ++ (List.map (\envItem -> "        - name: " ++ envItem.name ++ "\n          value: \"" ++ envItem.value ++ "\"\n") envItemList
                        |> List.foldr (++) ""
                   )

        Nothing ->
            ""


getSsoClientAsYaml : Console -> String
getSsoClientAsYaml console =
    case console.ssoClient of
        Just ssoClient ->
            "      ssoClient:\n"
                ++ "        name: "
                ++ ssoClient.name
                ++ "\n"
                ++ "        secret: "
                ++ ssoClient.secret
                ++ "\n"
                ++ "        hostnameHTTP: "
                ++ ssoClient.hostnameHttp
                ++ "\n"
                ++ "        hostnameHTTPS: "
                ++ ssoClient.hostnameHttps
                ++ "\n"

        Nothing ->
            ""
