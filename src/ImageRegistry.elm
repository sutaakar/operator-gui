module ImageRegistry exposing (ImageRegistry, Msg, emptyImageRegistry, getImageRegistryAsYaml, getImageRegistryView, mapImageRegistryEvent)

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



-- UPDATE


type Msg
    = ToggleInsecure
    | ChangeRegistryName String


mapImageRegistryEvent : Msg -> ImageRegistry -> ImageRegistry
mapImageRegistryEvent msg imageRegistry =
    case msg of
        ToggleInsecure ->
            { imageRegistry | insecure = not imageRegistry.insecure }

        ChangeRegistryName newRegistryName ->
            { imageRegistry | registry = newRegistryName }



-- VIEW


getImageRegistryView : ImageRegistry -> (Msg -> msg) -> List (Html msg)
getImageRegistryView imageRegistry msg =
    [ form []
        [ fieldset []
            [ legend [] [ text "Image registry configuration" ]
            , input [ type_ "checkbox", checked imageRegistry.insecure, onClick (msg ToggleInsecure) ] []
            , text "Insecure registry"
            , br [] []
            , text "Registry for Kie images: "
            , input [ placeholder "Registry", value imageRegistry.registry, onInput (ChangeRegistryName >> msg) ] []
            ]
        ]
    ]


getImageRegistryAsYaml : ImageRegistry -> String
getImageRegistryAsYaml imageRegistry =
    if String.length imageRegistry.registry > 0 then
        "  imageRegistry:\n"
            ++ getInsecureAsYaml imageRegistry
            ++ "    registry: "
            ++ imageRegistry.registry
            ++ "\n"

    else
        ""


getInsecureAsYaml : ImageRegistry -> String
getInsecureAsYaml imageRegistry =
    if imageRegistry.insecure then
        "    insecure: true\n"

    else
        "    insecure: false\n"
