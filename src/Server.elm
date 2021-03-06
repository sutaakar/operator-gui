module Server exposing (Msg, Server, emptyServer, getServerAsYaml, getServerView, mapServerEvent)

import EnvItem
import Html exposing (Attribute, Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import YamlUtils



-- MODEL


type From
    = ImageStreamTag String String
    | DockerImage String


type alias Build =
    { kieServerContainerDeployment : String
    , mavenMirrorUrl : String
    , artifactDir : String
    , uri : String
    , reference : String
    , contextDir : String
    }


type Database
    = H2 String
    | MySQL String
    | PostgreSQL String


type alias Server =
    { name : String
    , deployments : Maybe Int
    , replicas : Maybe Int
    , keystoreSecret : String
    , from : Maybe From
    , build : Maybe Build
    , env : Maybe (List EnvItem.EnvItem)
    , database : Maybe Database
    }


emptyBuild : Build
emptyBuild =
    { kieServerContainerDeployment = ""
    , mavenMirrorUrl = ""
    , artifactDir = ""
    , uri = ""
    , reference = ""
    , contextDir = ""
    }


emptyServer : Server
emptyServer =
    { name = ""
    , deployments = Nothing
    , replicas = Nothing
    , keystoreSecret = ""
    , from = Nothing
    , build = Nothing
    , env = Nothing
    , database = Nothing
    }


getDeploymentsAsString : Server -> String
getDeploymentsAsString server =
    case server.deployments of
        Just deployments ->
            String.fromInt deployments

        Nothing ->
            ""


getReplicasAsString : Server -> String
getReplicasAsString server =
    case server.replicas of
        Just replicas ->
            String.fromInt replicas

        Nothing ->
            ""


getFromFromName : String -> Maybe From
getFromFromName fromName =
    if fromName == "ImageStreamTag" then
        Just (ImageStreamTag "" "")

    else if fromName == "DockerImage" then
        Just (DockerImage "")

    else
        Nothing


getDatabaseFromName : String -> Maybe Database
getDatabaseFromName databaseName =
    case databaseName of
        "H2" ->
            Just (H2 "")

        "MySQL" ->
            Just (MySQL "")

        "PostgreSQL" ->
            Just (PostgreSQL "")

        _ ->
            Nothing



-- UPDATE


type Msg
    = ChangeName String
    | ChangeDeployments String
    | ChangeReplicas String
    | ChangeKeystoreSecret String
    | ChangeFrom String
    | ChangeFromName String
    | ChangeFromNamespace String
    | BuildMsg BuildMsg
    | EnvItemMsg EnvItem.Msg
    | ChangeDatabaseType String
    | ChangeDatabasePvcSize String


type BuildMsg
    = ChangeKieServerContainerDeployment String
    | ChangeMavenMirrorUrl String
    | ChangeArtifactDir String
    | ChangeGitUri String
    | ChangeGitReference String
    | ChangeGitContextDir String


mapServerEvent : Msg -> Server -> Server
mapServerEvent msg server =
    case msg of
        ChangeDeployments depl ->
            { server | deployments = String.toInt depl }

        ChangeFrom fromName ->
            { server | from = getFromFromName fromName }

        ChangeFromName newName ->
            case server.from of
                Just (ImageStreamTag _ namespace) ->
                    { server | from = Just (ImageStreamTag newName namespace) }

                Just (DockerImage _) ->
                    { server | from = Just (DockerImage newName) }

                _ ->
                    server

        ChangeFromNamespace newNamespace ->
            case server.from of
                Just (ImageStreamTag name namespace) ->
                    { server | from = Just (ImageStreamTag name newNamespace) }

                _ ->
                    server

        EnvItemMsg envItemMessage ->
            case server.env of
                Just envItems ->
                    { server | env = EnvItem.mapEnvItemEvent envItemMessage envItems }

                Nothing ->
                    { server | env = EnvItem.mapEnvItemEvent envItemMessage [] }

        ChangeName newName ->
            { server | name = newName }

        ChangeReplicas newReplicas ->
            { server | replicas = String.toInt newReplicas }

        ChangeKeystoreSecret newKeystoreSecret ->
            { server | keystoreSecret = newKeystoreSecret }

        BuildMsg buildMsg ->
            case server.build of
                Just build ->
                    { server | build = mapBuildMsg buildMsg build }

                Nothing ->
                    { server | build = mapBuildMsg buildMsg emptyBuild }

        ChangeDatabaseType newDatabaseType ->
            { server | database = getDatabaseFromName newDatabaseType }

        ChangeDatabasePvcSize newPersistentVolumeClaimSize ->
            { server
                | database =
                    case server.database of
                        Just (H2 _) ->
                            Just (H2 newPersistentVolumeClaimSize)

                        Just (MySQL _) ->
                            Just (MySQL newPersistentVolumeClaimSize)

                        Just (PostgreSQL _) ->
                            Just (PostgreSQL newPersistentVolumeClaimSize)

                        Nothing ->
                            Nothing
            }


mapBuildMsg : BuildMsg -> Build -> Maybe Build
mapBuildMsg msg build =
    case msg of
        ChangeKieServerContainerDeployment newKieServerContainerDeployment ->
            { build | kieServerContainerDeployment = newKieServerContainerDeployment } |> checkBuildContent

        ChangeMavenMirrorUrl newMavenMirrorUrl ->
            { build | mavenMirrorUrl = newMavenMirrorUrl } |> checkBuildContent

        ChangeArtifactDir newArtifactDir ->
            { build | artifactDir = newArtifactDir } |> checkBuildContent

        ChangeGitUri newUri ->
            { build | uri = newUri } |> checkBuildContent

        ChangeGitReference newReference ->
            { build | reference = newReference } |> checkBuildContent

        ChangeGitContextDir newContextDir ->
            { build | contextDir = newContextDir } |> checkBuildContent


checkBuildContent : Build -> Maybe Build
checkBuildContent build =
    if build /= emptyBuild then
        Just build

    else
        Nothing



-- VIEW


getServerView : (Msg -> msg) -> Server -> Bool -> List (Html msg)
getServerView msg server withDatabase =
    [ Html.fieldset []
        ([ Html.legend [] [ text "Configuration of the individual KIE server group" ]
         , div [] [ text "Server name: ", input [ placeholder "Server name", value server.name, onInput (ChangeName >> msg) ] [] ]
         , div [] [ text "Number of Kie server deployments: ", input [ placeholder "Deployments", value (getDeploymentsAsString server), onInput (ChangeDeployments >> msg) ] [] ]
         , div [] [ text "Kie server replicas for DeploymentConfig: ", input [ placeholder "DeploymentConfig replicas", value (getReplicasAsString server), onInput (ChangeReplicas >> msg) ] [] ]
         , div [] [ text "Keystore secret name: ", input [ placeholder "Keystore secret name", value server.keystoreSecret, onInput (ChangeKeystoreSecret >> msg) ] [] ]
         ]
            ++ getFromView server.from msg
            ++ getBuildView server msg
            ++ getEnvVariableView msg server
            ++ (if withDatabase then
                    getDatabaseView server msg

                else
                    []
               )
        )
    ]


getFromView : Maybe From -> (Msg -> msg) -> List (Html msg)
getFromView selectedFrom msg =
    case selectedFrom of
        Nothing ->
            [ select [ onInput (ChangeFrom >> msg) ] (getFromOptions selectedFrom) ]

        Just (ImageStreamTag name namespace) ->
            [ select [ onInput (ChangeFrom >> msg) ] (getFromOptions selectedFrom)
            , text "Name: "
            , input [ placeholder "Name", value name, onInput (ChangeFromName >> msg) ] []
            , text "Namespace: "
            , input [ placeholder "Keep empty for default", value namespace, onInput (ChangeFromNamespace >> msg) ] []
            ]

        Just (DockerImage name) ->
            [ select [ onInput (ChangeFrom >> msg) ] (getFromOptions selectedFrom)
            , text "Name: "
            , input [ placeholder "Name", value name, onInput (ChangeFromName >> msg) ] []
            ]


getFromOptions : Maybe From -> List (Html msg)
getFromOptions selectedFrom =
    case selectedFrom of
        Nothing ->
            [ option [ Html.Attributes.selected True, value "" ] [ text "" ]
            , option [ Html.Attributes.selected False, value "ImageStreamTag" ] [ text "ImageStreamTag" ]
            , option [ Html.Attributes.selected False, value "DockerImage" ] [ text "DockerImage" ]
            ]

        Just (ImageStreamTag _ _) ->
            [ option [ Html.Attributes.selected False, value "" ] [ text "" ]
            , option [ Html.Attributes.selected True, value "ImageStreamTag" ] [ text "ImageStreamTag" ]
            , option [ Html.Attributes.selected False, value "DockerImage" ] [ text "DockerImage" ]
            ]

        Just (DockerImage _) ->
            [ option [ Html.Attributes.selected False, value "" ] [ text "" ]
            , option [ Html.Attributes.selected False, value "ImageStreamTag" ] [ text "ImageStreamTag" ]
            , option [ Html.Attributes.selected True, value "DockerImage" ] [ text "DockerImage" ]
            ]


getBuildView : Server -> (Msg -> msg) -> List (Html msg)
getBuildView server msg =
    [ Html.fieldset []
        ([ Html.legend [] [ text "S2I configuration" ] ]
            ++ (case server.build of
                    Just build ->
                        [ br [] []
                        , text "The Maven GAV to deploy: "
                        , input [ placeholder "Maven GAV", value build.kieServerContainerDeployment, onInput (ChangeKieServerContainerDeployment >> BuildMsg >> msg) ] []
                        , br [] []
                        , text "The Maven mirror URL: "
                        , input [ placeholder "Maven mirror", value build.mavenMirrorUrl, onInput (ChangeMavenMirrorUrl >> BuildMsg >> msg) ] []
                        , br [] []
                        , text "List of artifact directories: "
                        , input [ placeholder "Artifact directories", value build.artifactDir, onInput (ChangeArtifactDir >> BuildMsg >> msg) ] []
                        , br [] []
                        , text "Git URI: "
                        , input [ placeholder "Git URI", value build.uri, onInput (ChangeGitUri >> BuildMsg >> msg) ] []
                        , br [] []
                        , text "Git repository branch: "
                        , input [ placeholder "Git repository branch", value build.reference, onInput (ChangeGitReference >> BuildMsg >> msg) ] []
                        , br [] []
                        , text "Code context/subdirectory: "
                        , input [ placeholder "Code context/subdirectory", value build.contextDir, onInput (ChangeGitContextDir >> BuildMsg >> msg) ] []
                        ]

                    Nothing ->
                        [ br [] [], text "The Maven GAV to deploy: ", input [ placeholder "Maven GAV", onInput (ChangeKieServerContainerDeployment >> BuildMsg >> msg) ] [] ]
               )
        )
    ]


getEnvVariableView : (Msg -> msg) -> Server -> List (Html msg)
getEnvVariableView msg server =
    case server.env of
        Just envItems ->
            EnvItem.getEnvVariableView (EnvItemMsg >> msg) envItems

        Nothing ->
            [ EnvItem.getLastEnvVariableView (EnvItemMsg >> msg) ]


getDatabaseView : Server -> (Msg -> msg) -> List (Html msg)
getDatabaseView server msg =
    case server.database of
        Nothing ->
            [ text "Database type: ", select [ onInput (ChangeDatabaseType >> msg) ] (getDatabaseOptions True False False False) ]

        Just (H2 persistentVolumeClaimSize) ->
            [ text "Database type: "
            , select [ onInput (ChangeDatabaseType >> msg) ] (getDatabaseOptions False True False False)
            , br [] []
            , text "Size of the PersistentVolumeClaim: "
            , input [ placeholder "PersistentVolumeClaim size", value persistentVolumeClaimSize, onInput (ChangeDatabasePvcSize >> msg) ] []
            ]

        Just (MySQL persistentVolumeClaimSize) ->
            [ text "Database type: "
            , select [ onInput (ChangeDatabaseType >> msg) ] (getDatabaseOptions False False True False)
            , br [] []
            , text "Size of the PersistentVolumeClaim: "
            , input [ placeholder "PersistentVolumeClaim size", value persistentVolumeClaimSize, onInput (ChangeDatabasePvcSize >> msg) ] []
            ]

        Just (PostgreSQL persistentVolumeClaimSize) ->
            [ text "Database type: "
            , select [ onInput (ChangeDatabaseType >> msg) ] (getDatabaseOptions False False False True)
            , br [] []
            , text "Size of the PersistentVolumeClaim: "
            , input [ placeholder "PersistentVolumeClaim size", value persistentVolumeClaimSize, onInput (ChangeDatabasePvcSize >> msg) ] []
            ]


getDatabaseOptions : Bool -> Bool -> Bool -> Bool -> List (Html msg)
getDatabaseOptions emptySelected h2Selected mySqlSelected postgreSqlSelected =
    [ option [ Html.Attributes.selected emptySelected, value "" ] [ text "" ]
    , option [ Html.Attributes.selected h2Selected, value "H2" ] [ text "H2" ]
    , option [ Html.Attributes.selected mySqlSelected, value "MySQL" ] [ text "MySQL" ]
    , option [ Html.Attributes.selected postgreSqlSelected, value "PostgreSQL" ] [ text "PostgreSQL" ]
    ]



-- YAML


getServerAsYaml : Server -> Int -> String
getServerAsYaml server intendation =
    YamlUtils.getNameAndNonEmptyValueWithIntendation "name" server.name (intendation + 1)
        ++ getDeploymentsAsYaml server (intendation + 1)
        ++ getReplicasAsYaml server (intendation + 1)
        ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "keystoreSecret" server.keystoreSecret (intendation + 1)
        ++ getFromAsYaml server (intendation + 1)
        ++ getBuildAsYaml server (intendation + 1)
        ++ getEnvAsYaml server (intendation + 1)
        ++ getDatabaseAsYaml server (intendation + 1)
        |> String.dropLeft ((intendation + 1) * 2)
        |> String.append (String.repeat intendation "  " ++ "- ")


getDeploymentsAsYaml : Server -> Int -> String
getDeploymentsAsYaml server intendation =
    case server.deployments of
        Just int ->
            YamlUtils.getNameAndValueWithIntendation "deployments" (String.fromInt int) intendation

        Nothing ->
            ""


getReplicasAsYaml : Server -> Int -> String
getReplicasAsYaml server intendation =
    case server.replicas of
        Just replicas ->
            YamlUtils.getNameAndValueWithIntendation "replicas" (String.fromInt replicas) intendation

        Nothing ->
            ""


getFromAsYaml : Server -> Int -> String
getFromAsYaml server intendation =
    case server.from of
        Just (ImageStreamTag name namespace) ->
            YamlUtils.getNameWithIntendation "from" intendation
                ++ YamlUtils.getNameAndValueWithIntendation "kind" "ImageStreamTag" (intendation + 1)
                ++ YamlUtils.getNameAndValueWithIntendation "name" name (intendation + 1)
                ++ (if String.length namespace > 0 then
                        YamlUtils.getNameAndValueWithIntendation "namespace" namespace (intendation + 1)

                    else
                        ""
                   )

        Just (DockerImage name) ->
            YamlUtils.getNameWithIntendation "from" intendation
                ++ YamlUtils.getNameAndValueWithIntendation "kind" "DockerImage" (intendation + 1)
                ++ YamlUtils.getNameAndValueWithIntendation "name" name (intendation + 1)

        Nothing ->
            ""


getBuildAsYaml : Server -> Int -> String
getBuildAsYaml server intendation =
    case server.build of
        Just build ->
            YamlUtils.getNameWithIntendation "build" intendation
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "kieServerContainerDeployment" build.kieServerContainerDeployment (intendation + 1)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "mavenMirrorURL" build.mavenMirrorUrl (intendation + 1)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "artifactDir" build.artifactDir (intendation + 1)
                ++ YamlUtils.getNameWithIntendation "gitSource" (intendation + 1)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "uri" build.uri (intendation + 2)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "reference" build.reference (intendation + 2)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "contextDir" build.contextDir (intendation + 2)

        Nothing ->
            ""


getEnvAsYaml : Server -> Int -> String
getEnvAsYaml server intendation =
    case server.env of
        Just envItems ->
            EnvItem.getEnvAsYaml envItems intendation

        Nothing ->
            ""


getDatabaseAsYaml : Server -> Int -> String
getDatabaseAsYaml server intendation =
    case server.database of
        Just (H2 persistentVolumeClaimSize) ->
            YamlUtils.getNameWithIntendation "database" intendation
                ++ YamlUtils.getNameAndValueWithIntendation "type" "h2" (intendation + 1)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "size" persistentVolumeClaimSize (intendation + 1)

        Just (MySQL persistentVolumeClaimSize) ->
            YamlUtils.getNameWithIntendation "database" intendation
                ++ YamlUtils.getNameAndValueWithIntendation "type" "mysql" (intendation + 1)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "size" persistentVolumeClaimSize (intendation + 1)

        Just (PostgreSQL persistentVolumeClaimSize) ->
            YamlUtils.getNameWithIntendation "database" intendation
                ++ YamlUtils.getNameAndValueWithIntendation "type" "postgresql" (intendation + 1)
                ++ YamlUtils.getNameAndNonEmptyValueWithIntendation "size" persistentVolumeClaimSize (intendation + 1)

        Nothing ->
            ""
