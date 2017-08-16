// Step 0. Boilerplate to get the paket.exe tool
 
open System
open System.IO
 
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
 
if not (File.Exists "paket.exe") then let url = "https://github.com/fsprojects/Paket/releases/download/0.31.5/paket.exe" in use wc = new Net.WebClient() in let tmp = Path.GetTempFileName() in wc.DownloadFile(url, tmp); File.Move(tmp,Path.GetFileName url); 

#r "paket.exe"
 
Paket.Dependencies(__SOURCE_DIRECTORY__ + "/paket.dependencies").Install(false,false)


//-----------------------------------------------------------------------------------------
#I "packages/FSCL.Compiler/lib/net45/"
#r "packages/FSCL.Compiler/lib/net45/FSCL.Compiler.Core.dll"
#r "packages/FSCL.Compiler/lib/net45/FSCL.Compiler.dll"
#r "packages/FSCL.Compiler/lib/net45/FSCL.Compiler.Language.dll"
#I "packages/FSCL.Runtime/lib/net451/"
#r "packages/FSCL.Runtime/lib/net451/FSCL.Runtime.dll"
#r "packages/FSCL.Runtime/lib/net451/FSCL.Runtime.Core.dll"
#r "packages/FSCL.Runtime/lib/net451/FSCL.Runtime.Execution.dll"
#r "packages/FSCL.Runtime/lib/net451/FSCL.Runtime.Language.dll"
#r "packages/FSCL.Runtime/lib/net451/FSCL.Runtime.CompilerSteps.dll"

open FSCL
open FSCL.Compiler
open FSCL.Language
open FSCL.Runtime

[<ReflectedDefinition>]
let VectorAdd(a: float32[], b:float32[], c:float32[], wi: WorkItemInfo) =
    let myId = wi.GlobalID(0)
    c.[myId] <- a.[myId] + b.[myId]
    c


// Instantiate the compiler
let compiler = new Compiler()
// Pass a kernel
let resultCompilingRef = compiler.Compile(<@ VectorAdd @>)
// Or a kernel call
let a = Array.create 1024 2.0f
let b = Array.create 1024 3.0f
let c = Array.zeroCreate<float32> 1024
let size = WorkSize(a.LongLength, 64L)
let resultCompilingCall = compiler.Compile(<@ VectorAdd(a, b, c, size) @>)

<@ VectorAdd(a, b, c, size) @>.Run(RunningMode.Sequential)
