//open Android.App
//open Android.Content
//open Android.OS
//open Android.Runtime
//open Android.Views
//open Android.Widget
namespace FMetrics.ViewModels

open System
open System.Threading
open System.Threading.Tasks


open Xamarin.Essentials
open FMetrics.BReusable


open WpfHelpers

type [<Measure>] ms

//[<AbstractClass>]
//type IDependencyService =
//    abstract member Get<'t> : unit -> 't
type DependencyService = Xamarin.Forms.DependencyService


//public interface IDataStore<T>
[<AbstractClass>]
type IDataStore<'t> =
    // Task<bool> AddItemAsync(T item);
    abstract member AddItemAsync: item:'t -> Task<bool>
    //    Task<bool> UpdateItemAsync(T item);
    abstract member UpdateItemAsync: item:'t -> Task<bool>
    //    Task<bool> DeleteItemAsync(string id);
    abstract member DeleteItemAsync: id:string -> Task<bool>
    //    Task<T> GetItemAsync(string id);
    abstract member GetItemAsync: id:string -> Task<'t>
    //    Task<IEnumerable<T>> GetItemsAsync(bool forceRefresh = false);
    abstract member GetItemsAsync: forceRefresh:bool -> Task<'t seq>
    abstract member GetItemsAsync: unit -> Task<'t seq>


type Item = {Id:string; Text:string; Description:string}

[<AbstractClass>]
//public class BaseViewModel : INotifyPropertyChanged
type BaseViewModel() as self =
    let propertyChanged = new Event<_, _>()
    let createInpc name defaultValue (options:Inpc.InpcOptions<'t> option) : Inpc.InpcWrapper<'t> =
        let opts: Inpc.InpcOptions<'t> option =
            options
            |> Option.defaultValue Inpc.InpcOptions.empty
            |> fun x -> {x with Inpc.InpcOptions.FComparerOpt=Some (=)}
            |> Some
        Inpc.createInpc self propertyChanged name defaultValue opts

    //public IDataStore<Item> DataStore => DependencyService.Get<IDataStore<Item>>();
    let dataStore = DependencyService.Get<IDataStore<Item>> ()
    let isBusy = createInpc "IsBusy" false None
    let title = createInpc "Title" String.Empty None

    member this.IsBusy with get() = isBusy.Value and set v = isBusy.Value <- v
    member this.Title with get() = title.Value and set v = title.Value <- v

    interface System.ComponentModel.INotifyPropertyChanged with
        [<CLIEvent>]
        member __.PropertyChanged = propertyChanged.Publish
    abstract member RaisePropertyChanged : string -> unit
    default x.RaisePropertyChanged(propertyName : string) = propertyChanged.Trigger(x, System.ComponentModel.PropertyChangedEventArgs(propertyName))
    member __.PropertyChanged = propertyChanged
module GeodudeLogic =
    let tryGetLocation() =
        eprintfn "geo, dude?"
        async {
            try
                match! Async.AwaitTask <| Geolocation.GetLastKnownLocationAsync() with
                | null ->
                    eprintfn "No geo, dude"
                    return Error "No location found"
                | location ->
                    return Ok location
            with
            | :? FeatureNotSupportedException as fns ->
                let name = "FeatureNotSupported"
                eprintfn "%s:%s" name fns.Message
                return Error name
            | :? FeatureNotEnabledException as fne ->
                Logging.logEx fne
                return Error "FeatureNotEnabled"
            | :? PermissionException as pex ->
                Logging.logEx pex
                return Error "PermissionException"
            | ex ->
                Logging.logEx ex
                return Error ex.Message
        }
    let runForever (sleep:int<ms>) f (t:CancellationToken) =
        let aTask =
            async {
                while not t.IsCancellationRequested do
                    do! Async.Sleep(millisecondsDueTime= int sleep)
                    if t.IsCancellationRequested then
                        return ()
                    f()
            }
        Async.Start(aTask,t)

type GeodudeFViewModel() as self =
    inherit BaseViewModel()
    let propChanged = base.PropertyChanged
    let createInpc name defaultValue (options:Inpc.InpcOptions<'t> option) : Inpc.InpcWrapper<'t> = Inpc.createInpc self propChanged name defaultValue options

    let latitude = createInpc "Latitude" String.Null None
    let longitude = createInpc "Longitude" String.Null None
    let error = createInpc "Error" String.Null None
    let tokenSource = new CancellationTokenSource()
    let mutable disposed = false
    let cleanup(disposing:bool) =
        if not disposed then
            disposed <- true
            if disposing then
                // cleanup managed resources
                tokenSource.Cancel()
                tokenSource.Dispose()
                ()
            // cleanup unmanaged resources
            ()

    do
        base.Title <- "Gps"
        printfn "Creating a geo vm"
        self.Init()
        GeodudeLogic.runForever 500<ms> (fun () ->
            self.Init()
        ) tokenSource.Token

    member __.Latitude
        with get () = latitude.Value
        and set v = latitude.Value <- v
    member __.Longitude
        with get () = longitude.Value
        and set v = longitude.Value <- v
    member __.Error
        with get () = error.Value
        and set v = error.Value <- v

    member this.Init() =
        GeodudeLogic.tryGetLocation()
        |> Async.RunSynchronously
        |> function
            |Ok location ->
                this.Error <- String.Empty
                this.Latitude <- string location.Latitude
                this.Longitude <- string location.Longitude
            |Error e ->
                this.Error <- e

    member this.Dispose()=
        cleanup(true)
        GC.SuppressFinalize(this)

    interface IDisposable with
        member x.Dispose() = x.Dispose()
    override __.Finalize() =
        cleanup(false)
