module EnvItem exposing (EnvItem, Msg, getEnvAsYaml, getEnvVariableView, getLastEnvVariableView, mapEnvItemEvent)

import Html exposing (Attribute, Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import YamlUtils



-- MODEL


type alias EnvItem =
    { name : String
    , value : String
    }



-- UPDATE


type Msg
    = AddNewEnvVariable String
    | UpdateExistingEnvVariableName EnvItem (List EnvItem) String
    | UpdateExistingEnvVariableValue EnvItem (List EnvItem) String


mapEnvItemEvent : Msg -> List EnvItem -> Maybe (List EnvItem)
mapEnvItemEvent msg envItems =
    case msg of
        AddNewEnvVariable newEnvVariableName ->
            Just (envItems ++ [ { name = newEnvVariableName, value = "" } ])

        UpdateExistingEnvVariableName updatedEnvVariable envVariables newEnvVariableName ->
            case updateEnvItemInList updatedEnvVariable (\envItem -> { envItem | name = newEnvVariableName }) envVariables of
                [] ->
                    Nothing

                xs ->
                    Just xs

        UpdateExistingEnvVariableValue updatedEnvVariable envVariables newEnvVariableValue ->
            Just (updateEnvItemInList updatedEnvVariable (\envItem -> { envItem | value = newEnvVariableValue }) envVariables)


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


getEnvVariableView : (Msg -> msg) -> List EnvItem -> List (Html msg)
getEnvVariableView msg envItems =
    List.map (getSingleEnvVariableView msg envItems) envItems ++ [ getLastEnvVariableView msg ]


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



-- YAML


getEnvAsYaml : List EnvItem -> Int -> String
getEnvAsYaml envItems intendation =
    YamlUtils.getNameWithIntendation "env" intendation
        ++ (List.map (\envItem -> YamlUtils.getNameAndValueWithDashAndIntendation "name" envItem.name (intendation + 2) ++ YamlUtils.getNameAndValueWithIntendation "value" envItem.value (intendation + 2)) envItems
                |> List.foldr (++) ""
           )
