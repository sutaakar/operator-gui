module Environment exposing (Environment, environments, rhdm_trial, getEnvironmentName, getEnvironmentFromName, getEnvironmentDropdownList)

import Html exposing (Html, Attribute, text, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MODEL

type Environment
  = Rhdm_authoring_ha String
  | Rhdm_authoring String
  | Rhdm_optaweb_trial String
  | Rhdm_production_immutable String
  | Rhdm_trial String
  | Rhpam_authoring_ha String
  | Rhpam_authoring String
  | Rhpam_production_immutable String
  | Rhpam_production String
  | Rhpam_trial String

rhdm_authoring_ha : Environment
rhdm_authoring_ha = Rhdm_authoring_ha "rhdm-authoring-ha"

rhdm_authoring : Environment
rhdm_authoring = Rhdm_authoring "rhdm-authoring"

rhdm_optaweb_trial : Environment
rhdm_optaweb_trial = Rhdm_optaweb_trial "rhdm-optaweb-trial"

rhdm_production_immutable : Environment
rhdm_production_immutable = Rhdm_production_immutable "rhdm-production-immutable"

rhdm_trial : Environment
rhdm_trial = Rhdm_trial "rhdm-trial"

rhpam_authoring_ha : Environment
rhpam_authoring_ha = Rhpam_authoring_ha "rhpam-authoring-ha"

rhpam_authoring : Environment
rhpam_authoring = Rhpam_authoring "rhpam-authoring"

rhpam_production_immutable : Environment
rhpam_production_immutable = Rhpam_production_immutable "rhpam-production-immutable"

rhpam_production : Environment
rhpam_production = Rhpam_production "rhpam-production"

rhpam_trial : Environment
rhpam_trial = Rhpam_trial "rhpam-trial"


environments : List Environment
environments = [rhdm_authoring_ha, rhdm_authoring, rhdm_optaweb_trial, rhdm_production_immutable, rhdm_trial, rhpam_authoring_ha, rhpam_authoring, rhpam_production_immutable, rhpam_production, rhpam_trial]

getEnvironmentName : Environment -> String
getEnvironmentName environment =
    case environment of
      Rhdm_authoring_ha name ->
        name
      Rhdm_authoring name ->
        name
      Rhdm_optaweb_trial name ->
        name
      Rhdm_production_immutable name ->
        name
      Rhdm_trial name ->
        name
      Rhpam_authoring_ha name ->
        name
      Rhpam_authoring name ->
        name
      Rhpam_production_immutable name ->
        name
      Rhpam_production name ->
        name
      Rhpam_trial name ->
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
