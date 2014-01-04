open System
open System.Text
open System.Text.RegularExpressions
open System.Diagnostics

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
    member x.Combine(cmp1, cmp2) = x.Bind(cmp1, fun () -> cmp2)
    member x.For(collection:seq<_>, func) =
        let ie = collection.GetEnumerator()
        let rec whileLoop body = if ie.MoveNext() then x.Bind(body(), fun _ -> whileLoop body) else Result ()
        whileLoop (fun () -> let value = ie.Current in func value)
    member x.Zero() = Result ()

let cmp = new CmpBuilder()

let toLines (s: string) = s.Split([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList

let exec cmd arg =
    printfn "Executing: %s %s" cmd arg
    let si = new ProcessStartInfo(cmd, arg)
    si.UseShellExecute <- false
    si.RedirectStandardOutput <- true
    si.RedirectStandardError <- true
    let pr = new Process()
    pr.StartInfo <- si
    pr.Start() |> ignore
    let output = (pr.StandardOutput.ReadToEnd())
    let error  = (pr.StandardError.ReadToEnd())
    pr.WaitForExit()
    if pr.ExitCode = 0 then Completed (output, error) else Failed (pr.ExitCode)
    
let git arg = 
    match exec "git" arg with
    | Completed (o, e) -> Result (sprintf "%s\n%s" o e |> toLines)
    | Failed code      -> Error (sprintf "Git command %s returned code %i" arg code)

let regExp re (groupName: string) input = 
    let regex = new Regex(re)
    if regex.IsMatch input then 
        let m = regex.Match input
        Some(m.Groups.[groupName].Value)
    else None

let getCurrentBranch lines =
    match lines with
    | hd :: _ ->
        match regExp "# On branch (?<name>.+)" "name" hd with
        | Some(name) -> Result name
        | None       -> Error (sprintf "Cannot retrieve current branch name")
    | _ -> Error (sprintf "Cannot retrieve current branch name")

let isEverythingCommitted lines =
    match lines with
    | _ :: nk :: _ -> Result(nk = "nothing to commit, working directory clean")
    | _            -> Error("Can't parse the output of git status")

let checkout branch =
    let msg = sprintf "Switched to branch '%s'" branch
    let rec loopThroughLines lines =
        match lines with
        | hd :: tl -> if hd = msg then Result(true) else loopThroughLines tl
        | [] -> Error(sprintf "Failed to switch to %s branch" branch)
    cmp { let! output = git (sprintf "checkout %s" branch)
          return! loopThroughLines output }

let parseArgs args =
    if 0 = Array.length args then (None, [])
    else if 1 = Array.length args then (Some args.[0], [])
    else (Some args.[0], args |> Seq.skip 1 |> Seq.toList)

    
[<EntryPoint>]
let main argv = 
    printfn "git-pushto pushes changes from local development branch to other branches."
    printfn "usage: git pushto <mainbranch> [<anotherbranch1>, <anotherbranch2>, ...]"
    printfn ""
    let (mainBranch, altBranches) = parseArgs argv
    match mainBranch with
    | Some mainBranchName ->
        let result = 
            cmp {
                let! statusOutput = git "status"
                let! currentBranch = getCurrentBranch statusOutput
                let! _ = isEverythingCommitted statusOutput
                let! _ = checkout mainBranchName
                let! _ = git "pull"
                let! _ = checkout currentBranch
                let! _ = git (sprintf "rebase %s" mainBranchName)
                let! _ = checkout mainBranchName
                let! _ = git (sprintf "merge %s" currentBranch)
                let! res = git "push"
                let! _ = checkout currentBranch
                return res
            }
        printfn "%O" result
        0
    | None -> 
        printfn "Error: <mainbranch> has to be specified"
        1