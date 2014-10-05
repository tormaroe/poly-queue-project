module pqd

open System
open Nancy
open Nancy.Hosting.Self
open Nancy.Conventions


let (?) (this : obj) (prop : string) : obj =    
    (this :?> DynamicDictionary).[prop]
//
//type FsNancyDict( d:DynamicDictionary ) =
//    inherit DynamicDictionary()     
//    member x.Item
//        with get(n:string) =
//            d.[n].ToString()
//
//type FsNancyModule() =
//    inherit NancyModule()
//    member x.Resp (r:string) =
//        Response.op_Implicit(r)
//    member x.Get (s:string) (f:FsNancyDict->Response) =
//            base.Get.Item(s) <- (fun o -> f (new FsNancyDict((o :?> DynamicDictionary ))) |> box)



type ServerModule() as this =
    inherit NancyModule()
    do 
        printfn "initializing.."
        this.Get.["/"] <-
            fun _ ->
                printfn "root"
                failwith "root"
        this.Get.["/test"] <-
            fun parameters ->
                printfn "/test"
                failwith "error in test"
//                Response.op_Implicit("test") |> box
        this.Get.["/{queue}/count"] <-
            fun parameters ->
                printfn "/../count"
                failwith "error in count"
//                new Nancy.Responses.JsonResponse("hello", null) 
//                |> box



[<EntryPoint>]
let main argv = 
    let nancyHost = new NancyHost(new Uri("http://127.0.0.1:8080/"))
    nancyHost.Start()
    printfn "Press [Enter] to exit."
    Console.ReadLine() |> ignore
    nancyHost.Stop()
    0
