module WpfHelpers

open System
// encapsulate INPC such that, fields can hold INPC values
module Inpc =
    open System.ComponentModel
    let checkComparerOpt fComparerOpt oldValue newValue =
        match fComparerOpt with
        | Some f ->
            if f oldValue newValue then
                printfn "Value was the same, keeping value %A = %A" oldValue newValue
                None
            else
                printfn "Value was different changing value! %A <> %A" oldValue newValue
                Some newValue
        | None -> Some newValue


    let triggerPropChanged source (event:Event<PropertyChangedEventHandler,PropertyChangedEventArgs>) name () =
        event.Trigger(source, PropertyChangedEventArgs(name))
    [<NoComparison>]
    [<NoEquality>]
    type InpcOptions<'T> = {    FComparerOpt: ('T -> 'T -> bool) option;
                                FDisplayNameOpt: ('T -> string) option
                                FDebugStringifyOpt: ('T -> string) option}
    module InpcOptions =
        let empty = {
            FComparerOpt = None
            FDisplayNameOpt=None
            FDebugStringifyOpt=None
        }

    type InpcWrapper<'T> (fNotifier: unit -> unit, inpcArgs:InpcOptions<'T>, defaultValue:'T) =
        let mutable field = defaultValue
        let fStringify x =
            match inpcArgs.FDebugStringifyOpt with
            | Some f -> f x
            | None -> (box x) |> string
        // consider:
        //member x.UnsafeSet v = field <- v
        member val IsDebug = false with get,set
        member val DisplayName : string =
            inpcArgs.FDisplayNameOpt |> Option.map (fun f -> f field) |> function | None -> null | Some x -> x
            with get,set
        member x.Value
            with get() =
                if x.IsDebug then
                    printfn "Getting value from %s (%s)" x.DisplayName (fStringify field)
                field
            and set v =
                let oldValue = field
                let compareResult = checkComparerOpt inpcArgs.FComparerOpt field v
                // before the change show what's in v
                // after changing show what's in the field which should be the same as what came out from v
                let printDebug (o:'T) =
                    printfn "InpcWrapper%s (hasComparer %b, comparerSaysChangeValue %b) from %s to %s"
                        (if not <| isNull x.DisplayName then " " + x.DisplayName else String.Empty)
                        (Option.isSome inpcArgs.FComparerOpt)
                        (Option.isSome compareResult)
                        (fStringify oldValue) (fStringify o)
                if x.IsDebug then
                    printDebug v
                compareResult
                |> Option.iter(fun v ->
                    field <- v
                    inpcArgs.FDisplayNameOpt
                    |> Option.iter(fun f -> x.DisplayName <- f field)
                    x.Notify()
                    if x.IsDebug then
                        printDebug field
                )
        member __.Notify() = fNotifier()

    // instead of using a parent/base class: use this method!
    // must have a real source (self for instance *sadface* ) arg for wpf to listen
    let createInpc source event name defaultValue options =
        let fNotifier = triggerPropChanged source event name
        match options with
        | Some options -> InpcWrapper(fNotifier, options, defaultValue)
        | None -> InpcWrapper(fNotifier,{FComparerOpt=None; FDisplayNameOpt=None; FDebugStringifyOpt=None}, defaultValue)

    // sample class for the createInpc method above
    type InpcEventWrappedSample () as self =
        let propertyChanged = new Event<_, _>()
        let createInpc name options defaultValue : InpcWrapper<'t> = 
            createInpc self propertyChanged name options defaultValue
        let encapsulated = createInpc "Encapsulated" false None

        member __.Encapsulated
            with get() = encapsulated.Value
            and set v = if v <> encapsulated.Value then encapsulated.Value <- v

        interface INotifyPropertyChanged with
            [<CLIEvent>]
            member __.PropertyChanged = propertyChanged.Publish
        abstract member RaisePropertyChanged : string -> unit
        default x.RaisePropertyChanged(propertyName : string) = propertyChanged.Trigger(x, PropertyChangedEventArgs(propertyName))
        member __.PropertyChanged = propertyChanged
