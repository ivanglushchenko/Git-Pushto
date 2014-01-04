module Cmp

type GitResult =
    | Completed of (string * string)
    | Failed    of int

type CmpResult<'v> =
    | Result of 'v
    | Error  of string
    override x.ToString() = 
        match x with
        | Result r -> sprintf "Result: %O" r
        | Error r  -> sprintf "Error: %O" r

type CmpBuilder() =
    member x.Bind(res, f) =
        match res with
        | Result r -> f r
        | Error r  -> Error r
    member x.Return value = Result value
    member x.Delay t = t()
    member x.ReturnFrom value = value
    member x.Combine(cmp1, cmp2) = x.Bind(cmp1, fun _ -> cmp2)
    member x.For(collection:seq<_>, func) =
        let ie = collection.GetEnumerator()
        let rec whileLoop body = if ie.MoveNext() then x.Bind(body(), fun _ -> whileLoop body) else Result ()
        whileLoop (fun () -> let value = ie.Current in func value)
    member x.Zero() = Result ()

let cmp = new CmpBuilder()