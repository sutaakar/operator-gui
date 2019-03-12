module Console exposing (Console, Msg, emptyConsole, getConsoleAsYaml, getConsoleView, mapConsoleEvent)

import Html exposing (Attribute, Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import YamlUtils



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


getConsoleAsYaml : Console -> Int -> String
getConsoleAsYaml console intendation =
    YamlUtils.getNameWithIntendation "console" intendation
        ++ getEnvAsYaml console (intendation + 1)
        ++ getSsoClientAsYaml console (intendation + 1)


getEnvAsYaml : Console -> Int -> String
getEnvAsYaml console intendation =
    case console.env of
        Just envItemList ->
            YamlUtils.getNameWithIntendation "env" intendation
                ++ (List.map (\envItem -> YamlUtils.getNameAndValueWithDashAndIntendation "name" envItem.name (intendation + 2) ++ YamlUtils.getNameAndValueWithIntendation "value" envItem.value (intendation + 2)) envItemList
                        |> List.foldr (++) ""
                   )

        Nothing ->
            ""


getSsoClientAsYaml : Console -> Int -> String
getSsoClientAsYaml console intendation =
    case console.ssoClient of
        Just ssoClient ->
            YamlUtils.getNameWithIntendation "ssoClient" intendation
                ++ YamlUtils.getNameAndValueWithIntendation "name" ssoClient.name (intendation + 1)
                ++ YamlUtils.getNameAndValueWithIntendation "secret" ssoClient.secret (intendation + 1)
                ++ YamlUtils.getNameAndValueWithIntendation "hostnameHTTP" ssoClient.hostnameHttp (intendation + 1)
                ++ YamlUtils.getNameAndValueWithIntendation "hostnameHTTPS" ssoClient.hostnameHttps (intendation + 1)

        Nothing ->
            ""
