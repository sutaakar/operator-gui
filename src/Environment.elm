module Environment exposing (Environment, environments, trial, auth, prod, prod_immutable, getEnvironmentName, getEnvironmentFromName, getEnvironmentDropdownList)

import Html exposing (Html, Attribute, text, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MODEL

type Environment
  = Trial String
  | Auth String
  | Prod String
  | Prod_immutable String

trial : Environment
trial = Trial "trial"

auth : Environment
auth = Auth "authoring"

prod : Environment
prod = Prod "production"

prod_immutable : Environment
prod_immutable = Prod_immutable "production-immutable"

environments : List Environment
environments = [trial, auth, prod, prod_immutable]

getEnvironmentName : Environment -> String
getEnvironmentName environment =
    case environment of
      Trial name ->
        name
      Auth name ->
        name
      Prod name ->
        name
      Prod_immutable name ->
        name

getEnvironmentFromName : String -> Maybe Environment
getEnvironmentFromName environmentName =
    List.filter (\environment -> (getEnvironmentName environment) == environmentName) environments |> List.head

-- VIEW

getEnvironmentDropdownList : Environment -> (String -> msg) -> Html msg
getEnvironmentDropdownList selectedEnvironment msg =
  select [ onInput msg] ( List.map (toEnvironmentOption selectedEnvironment) environments )

toEnvironmentOption : Environment -> Environment -> Html msg
toEnvironmentOption selectedEnvironment environment =
  option [ Html.Attributes.selected (selectedEnvironment == environment), value (getEnvironmentName environment) ] [text (getEnvironmentName environment)]
