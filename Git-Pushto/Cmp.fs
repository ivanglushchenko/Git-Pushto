module Cmp

type CmpResult<'v> =
    | Result of 'v
    | Error of string
    | Delayed of Lazy<CmpResult<'v>>

    override x.ToString() = 
        match x with
        | Result r -> sprintf "Result: %O" r
        | Error r -> sprintf "Error: %O" r
        | Delayed t -> sprintf "Delayed: %O" t

type CmpBuilder() =
    member x.Bind(res, f) =
        match res with
        | Result r -> f r
        | Error r -> Error r
        | Delayed t -> Delayed (lazy ( x.Bind(t.Force(), f) ))
    member x.Return value = Result value
    member x.Delay (t: unit -> CmpResult<'v>) = Delayed (lazy (t()))
    member x.ReturnFrom value = value
    member x.Combine(cmp1, cmp2) = x.Bind(cmp1, fun _ -> cmp2)
    member x.For(collection:seq<_>, f: 'a -> CmpResult<'b>) =
        let enum = collection.GetEnumerator()
        let rec whileLoop body = if enum.MoveNext() then x.Bind(body, fun _ -> enum.Current |> f |> whileLoop) else body
        whileLoop (Result (Unchecked.defaultof<'b>))
    member x.Zero() = Result ()
    member x.Run(res) =
        match res with
        | Result r -> res
        | Error r -> res
        | Delayed t -> x.Run(t.Force())

let cmp = new CmpBuilder()