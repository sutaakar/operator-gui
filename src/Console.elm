module Console exposing (Console, Msg, emptyConsole, getConsoleAsYaml, getConsoleView, getConsoleViewEmpty, mapConsoleEvent)

import Html exposing (Attribute, Html, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MODEL


type alias EnvItem =
    { name : String
    }


type alias Console =
    { env : Maybe (List EnvItem)
    }


emptyConsole : Console
emptyConsole =
    { env = Nothing
    }



-- UPDATE


type Msg
    = AddNewEnvVariable String
    | UpdateExistingEnvVariableName EnvItem (List EnvItem) String


mapConsoleEvent : Msg -> Console -> Maybe Console
mapConsoleEvent msg console =
    case msg of
        AddNewEnvVariable newEnvVariableName ->
            case console.env of
                Just envItemList ->
                    Just { console | env = Just (envItemList ++ [ { name = newEnvVariableName } ]) }

                Nothing ->
                    Just { console | env = Just [ { name = newEnvVariableName } ] }

        UpdateExistingEnvVariableName updatedEnvVariable envVariables newEnvVariableName ->
            case updateEnvItemInList updatedEnvVariable (\envItem -> { envItem | name = newEnvVariableName }) envVariables of
                [] ->
                    Nothing

                xs ->
                    Just { console | env = Just xs }


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



-- VIEW


getConsoleView : (Msg -> msg) -> Console -> List (Html msg)
getConsoleView msg console =
    case console.env of
        Just envItemList ->
            List.map (getSingleEnvVariableView msg envItemList) envItemList ++ [ getLastEnvVariableView msg ]

        Nothing ->
            [ getLastEnvVariableView msg ]


getConsoleViewEmpty : (Msg -> msg) -> List (Html msg)
getConsoleViewEmpty msg =
    [ getLastEnvVariableView msg ]


getSingleEnvVariableView : (Msg -> msg) -> List EnvItem -> EnvItem -> Html msg
getSingleEnvVariableView msg envItemList envItem =
    div []
        [ text "Env variable name: "
        , input [ placeholder "Name", value envItem.name, onInput (UpdateExistingEnvVariableName envItem envItemList >> msg) ] []
        ]


getLastEnvVariableView : (Msg -> msg) -> Html msg
getLastEnvVariableView msg =
    div []
        [ text "Env variable name: "
        , input [ placeholder "Name", onInput (AddNewEnvVariable >> msg) ] []
        ]



-- YAML


getConsoleAsYaml : Console -> String
getConsoleAsYaml console =
    "    console:\n"
        ++ getEnvAsYaml console


getEnvAsYaml : Console -> String
getEnvAsYaml console =
    case console.env of
        Just envItemList ->
            "      env:\n"
                ++ (List.map (\envItem -> "        - name: " ++ envItem.name ++ "\n") envItemList
                        |> List.foldr (++) ""
                   )

        Nothing ->
            ""
