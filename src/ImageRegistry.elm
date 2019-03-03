module ImageRegistry exposing (ImageRegistry, emptyImageRegistry, getImageRegistryAsYaml)

import Html exposing (Attribute, Html, br, div, fieldset, form, input, legend, option, select, text)
import Html.Attributes exposing (checked, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)



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


getImageRegistryAsYaml : ImageRegistry -> String
getImageRegistryAsYaml imageRegistry =
    "  imageRegistry:\n"
        ++ getInsecureAsYaml imageRegistry
        ++ "    registry: "
        ++ imageRegistry.registry
        ++ "\n"


getInsecureAsYaml : ImageRegistry -> String
getInsecureAsYaml imageRegistry =
    if imageRegistry.insecure then
        "    insecure: true\n"

    else
        "    insecure: false\n"
