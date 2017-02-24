open Suave
open Suave.Http.Successful
open Suave.Http.Applicatives
open Suave.Http.Writers
open Suave.Http.Files
open Suave.Http
open Suave.Web
open Suave.Types
open System
open System.IO
open System.Collections.Generic

open Newtonsoft.Json

open Process

let ip = "127.0.0.1"
let port = "8083"
let mimeTypes =
    defaultMimeTypesMap
    >=> (function | ".epub" -> mkMimeType "application/epub+zip" true | _ -> None)

let getFile path =
    File.ReadAllText path

let safePath path =
    let safeChars = "-_().abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    let contains x =
        Seq.exists((=) x)
    let charSeq = (Seq.filter (fun x -> contains x safeChars) path)
    charSeq |> Seq.map string |> String.concat ""


let thoth x =
    let UUID = Guid.NewGuid().ToString("N").Substring(0, 7)
    let DownloadAndCreateEpub urlList title author cover strict =
        EbookFromList strict title author cover urlList UUID
    let ReqToString (req : HttpRequest) =
        System.Text.Encoding.UTF8.GetString(req.rawForm)

    let ReadUrls (req : HttpRequest) =
        let json = JsonConvert.DeserializeObject<List<string>>(ReqToString req)
        Seq.toList json
    let Remove3 li =
        let rec loop li acc counter =
           match li with
           | hd :: tl when counter < 4 -> loop tl acc (counter + 1)
           | hd :: tl -> loop tl (hd :: acc) (counter + 1)
           | _ -> acc
        loop li [] 0

    let json = ReadUrls x
    let title = List.item 0 json
    let author = List.item 1 json
    let cover = List.item 2 json
    let strict = 
        match (List.item 3 json) with
        |"true" -> false
        |"false" -> true
        |_ -> true
    printf "Strict mode: %A" strict 
    let urls = Remove3 json
    DownloadAndCreateEpub urls title author cover strict
    OK (sprintf "\"/thoth/files/%s\"" (UUID + "/" + (safePath title) + ".epub"))
    
let getPaths =
    [
        path "/" >>= OK (getFile "home/index.html")
        path "/thoth" >>= OK (getFile "elm/index.html")
        path "/hell" >>= OK (getFile "elm/haruhi.html")
        pathScan "/thoth/files/%s" (fun x -> file (IO.Path.Combine("./data", x)))
    ]

let postPaths =
    [
        path "/" >>= request (fun x -> OK "HELLO WORLD")
        path "/thoth" >>= request (fun x -> thoth x)
    ]

let app =
    choose [
        GET >>= choose getPaths;
        POST >>= choose postPaths
    ]

startWebServer {defaultConfig with mimeTypesMap = mimeTypes} app
