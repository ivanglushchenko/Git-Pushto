open System
open System.Text
open System.Text.RegularExpressions
open System.Diagnostics

open Cmp
open Git

let parseArgs args =
    if 0 = Array.length args then (None, [])
    else if 1 = Array.length args then (Some args.[0], [])
    else (Some args.[0], args |> Seq.skip 1 |> Seq.toList)

let pushto mainBranchName altBranches =
    cmp {
        let! statusOutput = git "status"
        let! currentBranch = getCurrentBranch statusOutput
        let! _ = isEverythingCommitted statusOutput
        let! commits = getDiff mainBranchName currentBranch
        let! _ = checkout mainBranchName
        let! _ = git "pull"
        let! _ = checkout currentBranch
        let! _ = git (sprintf "rebase %s" mainBranchName)
        let! _ = checkout mainBranchName
        let! _ = git (sprintf "merge %s" currentBranch)
        let! _ = git "push"
        let! _ = checkout currentBranch

        if List.isEmpty altBranches = false then
            for branch in altBranches do
                let! _ = checkout branch
                let! _ = git "pull"
                for commit in commits do
                    let! _ = git (sprintf "cherry-pick %s" commit)
                    let! _ = git "push"
                    ()
            let! _ = checkout currentBranch
            ()

        return "all done" }
    
[<EntryPoint>]
let main argv = 
    printfn "git-pushto pushes changes from local development branch to other branches."
    printfn "usage: git pushto <mainbranch> [<anotherbranch1>, <anotherbranch2>, ...]"
    printfn ""

    let (mainBranch, altBranches) = parseArgs argv
    match mainBranch with
    | Some mainBranchName ->
        let result = pushto mainBranchName altBranches
        printfn "%O" result
        0

    | None -> 
        printfn "Error: <mainbranch> has to be specified"
        1