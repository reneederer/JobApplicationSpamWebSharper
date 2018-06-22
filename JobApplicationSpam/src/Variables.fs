namespace JobApplicationSpam
module Variables =
    open Types
    open FParsec

    type AssignedVariable = string
    type MatchValue = string
    type MatchCompareVariable = string
    type LiteralString = string

    type StringDefinition =
    | LiteralString of string
    | ConcatenatedStrings of list<StringDefinition>

    type Expression =
    | MatchExpression of MatchCompareVariable * list<MatchValue * LiteralString>
    | VariableExpression of LiteralString

    type Definition = AssignedVariable * Expression

    let tryGetValue (expression : Expression) (definedVariables: list<string * string>) =
        match expression with
        | MatchExpression (matchCompareVariable, lines) ->
            let searchMatchValue = 
                definedVariables
                |> List.tryPick (fun (k, v) -> if k = matchCompareVariable then Some v else None)
                |> Option.defaultValue ""

            lines
            |> List.tryPick (fun (variableValue, literalString) ->
                if variableValue = searchMatchValue then Some literalString else None)
        | VariableExpression literalString ->
            Some literalString
        | _ -> None

    let spacedNewline =
        attempt
            (manyChars (anyOf [' '; '\t']))
            .>> newline
            .>> manyChars (anyOf [' '; '\t'])
    
    let spacesFollowedByNewline =
        attempt (many (spacedNewline .>> followedBy newline))
        |>> (fun xs -> System.String.Concat xs)
    
    let spacesNoNewline =
        manyChars (anyOf [' '; '\t'])
    
    let spacesTillNewline =
        spacesFollowedByNewline <|> spacesNoNewline


    let pVariable =
        spaces
        .>> pchar '$'
        >>. manyChars (noneOf [' '; '\t'; '\n'])
        |>> (fun x -> "$" + x)

    let pLiteralString =
        spaces
        >>. pchar '"'
        >>. (manyChars (noneOf ['"']))
        .>> pchar '"'
        .>> spacesTillNewline

    let pMatchCompareVariable =
        spaces
        >>. pstring "match"
        >>. spaces1
        >>. pVariable
        .>> spaces1
        .>> pstring "with"
        .>> spacesTillNewline

    let pMatchLines =
        spaces
        >>. pchar '|'
        >>. spaces
        >>. pLiteralString
        .>> spaces
        .>> pstring "->"
        .>> spaces
        .>>. pLiteralString

    let pConcatenatedStrings1 =
        spaces
        >>. sepBy1
                ((attempt (pVariable .>> spacesTillNewline))
                   <|> (attempt (pLiteralString .>> spacesTillNewline)))
                (attempt (pchar '+' .>> spaces))
        |>> (fun xs -> String.concat "" xs)

    let pAssignedVariable : Parser<AssignedVariable, unit> =
        spaces
        >>. pVariable

    let pMatchDefinition =
        spaces
        >>. pAssignedVariable
        .>> spaces
        .>> pchar '='
        .>> spaces
        .>>. pMatchCompareVariable
        .>> spacesTillNewline
        .>> newline
        .>> spaces
        .>>. (many1 (attempt (pMatchLines .>> spacesTillNewline)))
        |>> (fun ((assignedVariable, matchCompareVariable), kvList) ->
                assignedVariable, MatchExpression (matchCompareVariable, kvList)
            )

    let pVariableDefinition =
        spaces
        >>. pAssignedVariable
        .>> spaces
        .>> pchar '='
        .>> spaces
        .>>. pConcatenatedStrings1
        |>> fun (assignedVariable, literalString) ->
                assignedVariable, VariableExpression literalString

    let pDefinitions =
        (attempt pMatchDefinition <|> attempt pVariableDefinition)

    let parse str =
        match run pDefinitions str with
        | Success (result, _, _) -> Types.Ok result
        | Failure (result, _, _) -> Types.Failure "Couldn't parse"

    let parseMany strs =
        [ for str in strs do
            yield parse str
        ]




