module YamlUtils exposing (getNameAndValueWithDashAndIntendation, getNameAndValueWithIntendation, getNameWithIntendation)


getNameWithIntendation : String -> Int -> String
getNameWithIntendation name intendation =
    String.repeat intendation "  " ++ name ++ ":\n"


getNameAndValueWithIntendation : String -> String -> Int -> String
getNameAndValueWithIntendation name value intendation =
    String.repeat intendation "  " ++ name ++ ": " ++ value ++ "\n"


getNameAndValueWithDashAndIntendation : String -> String -> Int -> String
getNameAndValueWithDashAndIntendation name value intendation =
    String.repeat (intendation - 1) "  " ++ "- " ++ name ++ ": " ++ value ++ "\n"
