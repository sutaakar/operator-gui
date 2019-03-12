module ImageRegistry exposing (ImageRegistry, emptyImageRegistry, getImageRegistryAsYaml)

import Html exposing (Attribute, Html, br, div, fieldset, form, input, legend, option, select, text)
import Html.Attributes exposing (checked, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import YamlUtils



-- MODEL


type alias ImageRegistry =
    { insecure : Bool
    , registry : String
    }


emptyImageRegistry : ImageRegistry
emptyImageRegistry =
    { insecure = False
    , registry = ""
    }


getImageRegistryAsYaml : ImageRegistry -> Int -> String
getImageRegistryAsYaml imageRegistry intendation =
    YamlUtils.getNameWithIntendation "imageRegistry" intendation
        ++ getInsecureAsYaml imageRegistry (intendation + 1)
        ++ YamlUtils.getNameAndValueWithIntendation "registry" imageRegistry.registry (intendation + 1)


getInsecureAsYaml : ImageRegistry -> Int -> String
getInsecureAsYaml imageRegistry intendation =
    if imageRegistry.insecure then
        YamlUtils.getNameAndValueWithIntendation "insecure" "true" intendation

    else
        YamlUtils.getNameAndValueWithIntendation "insecure" "false" intendation
