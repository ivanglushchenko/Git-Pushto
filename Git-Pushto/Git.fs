module Git

open System
open System.Text
open System.Text.RegularExpressions
open System.Diagnostics
open Cmp

type GitResult =
    | Completed of (string * string)
    | Failed of string

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
    let error = (pr.StandardError.ReadToEnd())
    pr.WaitForExit()
    if pr.ExitCode = 0 then Completed (output, error)
    else if System.String.IsNullOrEmpty error then Failed (sprintf "Command %s %s returned code %i" cmd arg pr.ExitCode)
    else Failed (sprintf "Command %s %s returned code %i, error: %s" cmd arg pr.ExitCode error)

let inline git arg = 
    match exec "git" arg with
    | Completed (o, e) -> Result (sprintf "%s\n%s" o e |> toLines)
    | Failed code -> Error code

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
        | None -> Error (sprintf "Cannot retrieve current branch name")
    | _ -> Error (sprintf "Cannot retrieve current branch name")

let isEverythingCommitted lines =
    match lines with
    | _ :: nk :: _ -> Result(nk = "nothing to commit, working directory clean")
    | _ -> Error("Can't parse the output of git status")

let checkout branch =
    let msg = sprintf "Switched to branch '%s'" branch
    let rec loopThroughLines lines =
        match lines with
        | hd :: tl -> if hd = msg then Result(true) else loopThroughLines tl
        | [] -> Error(sprintf "Failed to switch to %s branch" branch)
    cmp { let! output = git (sprintf "checkout %s" branch)
          return! loopThroughLines output }

let getDiff main current =
    let rec extractCommitsShas lines acc = 
        match lines with
        | hd :: tl -> 
            match regExp "^commit (?<name>[a-z0-9]{40,40})$" "name" hd with
            | Some name -> extractCommitsShas tl (name :: acc)
            | None -> extractCommitsShas tl acc
        | [] -> acc
    cmp { let! log = git (sprintf "log %s..%s" main current)
    return extractCommitsShas log [] }