//------------------------------------------------
// HTML Table data type provider samples


#load "packages/FSharp.Charting/FSharp.Charting.fsx"
#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
open FSharp.Charting
open FSharp.Data

//------------------------------------------------
// Basic HTML Table data 


type MarketDepth = HtmlProvider<"https://www.bmreports.com/bmrs/?q=balancing/systemsellbuyprices">

let mrktDepthData = MarketDepth.GetSample()

let mrktDepthTable = mrktDepthData.Tables.MyTable1

//for row in mrktDepthTable.Rows do 
//   row.

// Look at the most recent row. Note the 'Date' property
// is of type 'DateTime' and 'Open' has a type 'decimal'
let firstRow = mrktDepthTable.Rows.[0]
let settlementDate = firstRow.``Settlement Date``
let acceptedBid = firstRow.ABV
let acceptedOffer = firstRow.ASV

// Print the bid / offer volumes for each row
for row in mrktDepthTable.Rows do
  printfn "Bid/Offer: (%A, %A, %A)" row.``Settlement Date`` row.ABV row.ASV

//--------------------------------------------------
// Downloading statistics

type NugetStats = HtmlProvider<"https://www.nuget.org/packages/FSharp.Data">

// load the live package stats for FSharp.Data
let rawStats = NugetStats().Tables.``Version History``

// helper function to analyze version numbers from nuget
let getMinorVersion (v:string) =  System.Text.RegularExpressions.Regex(@"\d.\d").Match(v).Value

// group by minor version and calculate download count
let stats = 
    rawStats.Rows
    |> Seq.groupBy (fun r -> getMinorVersion r.Version)
    |> Seq.sortBy fst
    |> Seq.map (fun (k, xs) -> k, xs |> Seq.sumBy (fun x -> x.Downloads))

// Visualize the package stats
Chart.Column stats

//-----------------------------------------------------







let doctorWho = new HtmlProvider<"http://en.wikipedia.org/wiki/List_of_Doctor_Who_serials">()

// Get the average number of viewers for each doctor
let viewersByDoctor = 
    doctorWho.Tables.``Series overview``.Rows 
    |> Seq.groupBy (fun row -> row.Doctor)
    |> Seq.map (fun (doctor, seasons) -> 
         doctor, seasons |> Seq.averageBy (fun season -> try float season.Episodes with _ -> 0.0))
    |> Seq.toArray

// Visualize it
Chart.Column(viewersByDoctor).WithYAxis(Title = "Episodes")

