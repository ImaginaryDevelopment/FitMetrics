namespace FMetrics.BReusable

open System

module Logging =
    // I think making it generic means getting the type name will be a thing that can potentially be done at compile time for certain paths
    let logEx<'t when 't : null and 't :> Exception> (ex:'t) =
        match ex with
        | null -> eprintfn "Exception logging called with null exception"
        //| ex -> eprintfn "%s: %s" (ex.GetType().Name) ex.Message
        | ex -> eprintfn "%s: %s" typeof<'t>.Name ex.Message

module String =
    let before (delimiter:string) (value:string) : string =
        if String.IsNullOrEmpty delimiter then
            invalidArg (nameof delimiter) "delimiter must be a value string"
        value.[0..value.IndexOf delimiter - 1]
    let Null:string = null

#if DEBUG

    module Samples =
        let beforef (delimiter:string) (value:string) : string option =
            if String.IsNullOrEmpty delimiter then
                None
            else
                Some value.[0..value.IndexOf delimiter - 1]
        let beforef2 (delimiter:string) (value:string) : Result<string,string> =
            if String.IsNullOrEmpty delimiter then
                Result.Error "bad delimiter"
            else
                Result.Ok value.[0..value.IndexOf delimiter - 1]

        type BeforeErrorType =
            | EmptyDelimiter
            | NullDelimiter
            | EmptyInput

        let beforef3 delimiter value : Result<string,BeforeErrorType> =
            if isNull delimiter then Error NullDelimiter
            elif delimiter = "" then Error BeforeErrorType.EmptyDelimiter
            elif value = "" then Error EmptyInput
            else Ok (value |> before delimiter)


    module Tests =
        let text = "hello world"
        // list of 3 items each is a tuple (delimiter,expected)
        [   " ", "hello"
            " world", "hello"
            "h", ""
        ]
        |> List.iter(fun (d,expected) ->
            //let actual = before d text
            let actual = text |> before d |> before "hello"
            if actual <> expected then invalidOp (sprintf "%s assertion failed for delim %s" (nameof before) d)
        )

module Blah = 
    // void runSample() {
    let runSample () =
        "hello world" |> String.before " "
#endif
