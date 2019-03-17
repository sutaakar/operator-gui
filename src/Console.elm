module Console exposing (Console, Msg, emptyConsole, getConsoleAsYaml, getConsoleView, mapConsoleEvent)

import EnvItem
import Html exposing (Attribute, Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import YamlUtils



-- MODEL


type alias SsoClient =
    { name : String
    , secret : String
    , hostnameHttp : String
    , hostnameHttps : String
    }


type alias Console =
    { env : Maybe (List EnvItem.EnvItem)
    , ssoClient : Maybe SsoClient
    }


emptyConsole : Console
emptyConsole =
    { env = Nothing
    , ssoClient = Nothing
    }



-- UPDATE


type Msg
    = EnvItemMsg EnvItem.Msg
    | AddNewSssoClientName String
    | UpdateExistingSsoClientName SsoClient String
    | UpdateExistingSsoSecret SsoClient String
    | UpdateExistingSsoHostnameHttp SsoClient String
    | UpdateExistingSsoHostnameHttps SsoClient String


mapConsoleEvent : Msg -> Console -> Maybe Console
mapConsoleEvent msg console =
    case msg of
        EnvItemMsg envItemMessage ->
            case console.env of
                Just envItems ->
                    { console | env = EnvItem.mapEnvItemEvent envItemMessage envItems } |> checkConsoleContent

                Nothing ->
                    Just { console | env = EnvItem.mapEnvItemEvent envItemMessage [] }

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
        Just envItems ->
            EnvItem.getEnvVariableView (EnvItemMsg >> msg) envItems

        Nothing ->
            [ EnvItem.getLastEnvVariableView (EnvItemMsg >> msg) ]


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
        Just envItems ->
            EnvItem.getEnvAsYaml envItems intendation

        Nothing ->
            ""


getSsoClientAsYaml : Console -> Int -> String
getSsoClientAsYaml console intendation =
    case console.ssoClient of
        Just ssoClient ->
            YamlUtils.getNameWithIntendation "ssoClient" intendation
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "name" ssoClient.name (intendation + 1)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "secret" ssoClient.secret (intendation + 1)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "hostnameHTTP" ssoClient.hostnameHttp (intendation + 1)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "hostnameHTTPS" ssoClient.hostnameHttps (intendation + 1)

        Nothing ->
            ""
