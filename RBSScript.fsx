
//------------------------------------
// This demo script is one of the interim results of an explorative joint project
// between RBS and Microsoft Research from October 2015 to early 2016 involving
// a part-time interaction between Don Syme (MSR), Karim Roshd and Fuzz Pezeshkpour.
//
//   Email: farzad.pezeshkpour@rbs.com, karim.roshd@rbs.com
//
// The topic we have been investigating is an end-to-end workflow using the F# language
// to draw trade data from Data Fabric using the innovative "F# Type Providers" feature, 
// price it using CAF and scale out computations using MBrace, a framework for distributed
// scale-out computations using F# and/or C#.
//
// F# is a Microsoft=supported strongly typed functional programming language. In this session 
// we will be using F# as a data exploration and compute language using F# Interactive, the 
// dynamic REPL compiler for F#. You can find out more about F# at http://fsharp.org.  
//
// MBrace is a framework for distributed scale-out computations using F# and/or C# and allows
// you to scale out directly from F# Interactive.  You can also 
// use MBrace from compiled code.  You can find out more about MBrace at http://mbrace.io.
//
// This has high potential value for RBS for the following reasons:
//
//   - Explorative scale-out of quant calculations is of crucial importance to RBS's
//     ability to address new regulator requirements and scenarios quickly and adaptively.
//
//   - F# is a highly productivity language used by other finance institutions 
//     such as Credit Suisse that is under utilized at RBS.
//
//   - F#'s data access facilities are particularly interesting in the context of Data Fabric.
//
//   - MBrace's scale out facilities are intuitive and particularly easy to use.
//
//
// OK, let's get started.

// These references allow us to access MBrace, CAF and DF
#load "ThespianCluster.fsx"
#load "CAFWrapper.fs"
#load "PvDemo.fs"

#r @"C:\Various\vs\platform\client\clr\DataFabric.FSharp/packages/DataFabric.NET.2.5.0.0/lib/net40/DataFabric.NET.dll"
#r @"C:\Various\vs\platform\client\clr\DataFabric.FSharp/bin/Debug/DataFabric.FSharp.dll"


open System.IO
open MBrace.Core
open MBrace.Flow
open PvDemo
open DataFabric.FSharp


//------------------------------------
// Demo part 1: F# + CAF



// Micro Example 1 - Load the market data into CAF
CAF.LoadMarketData @"C:\Various\trades\mktdata4.fv"


// Micro Example 2 - Compute PV for a trade using CAF
let pvForStrike (strike, marketDataFile) = 

    let mdeName = "capMarketData"
    CAF.LoadMarketData marketDataFile
    let trade = CAF.BuildTrade ("cap1", mdeName, strike)
    CAF.PvTrade (trade, mdeName)

pvForStrike (0.01, @"C:\Various\trades\mktdata1.fv")

// Micro Example 3 - Compute PV for a range of strikes using CAF
let strikes = 
   [ (0.01, @"C:\Various\trades\mktdata1.fv") 
     (0.02, @"C:\Various\trades\mktdata2.fv")  
     (0.04, @"C:\Various\trades\mktdata3.fv")  
     (0.08, @"C:\Various\trades\mktdata4.fv") ] 


let pvs1 = 
    [ for (strike, mdf) in strikes -> pvForStrike (strike, mdf) ] 


//------------------------------------
// Demo part 2: F# + CAF + MBrace
//
// In this part, we run CAF computations over a cluster of MBrace-enabled machines.
// These machines could be in the existing RBS grid or VMs in any grid infrastructure.
// 
// MBrace can be used in an explorative way - you can investigate how long jobs take to run,
// what sort of resources they use, what sort of accuracy is achieved.

let cluster = Config.GetCluster()

// Start the job...  We are now running CAF computations over a distributed cluster of 
// machines. (In this case, the cluster is locally simulated, but it could be running on 
// any data center or cloud fabric).
let pvsJob = 
    [ for (strike, mdf) in strikes -> local { return pvForStrike (strike, mdf) } ] 
    |> Cloud.Parallel 
    |> cluster.CreateProcess

// Get the results
let pvs = pvsJob.Result





//------------------------------------
// Demo part 3 – F# + DF
//
// In this section, we switch back and look at F# plus Data Fabric.
//
// F# can use the DataFabric.NET libraries directly just like C#.  However that
// kind of data access is schema-less.  As part of our experiment we created
// an F# type provider that infers schema for any Data Fabric collection based on 
// a sampling of documents in that collection.  An F# type provider is an adaptor that infers
// types based on schema or data.   Inferring types from data samples is particularly useful 
// when prototyping with DF-like systems that are schemaless document stores.  
//
// The design of the F# type provider could easily be extended to incorporate any more fixed
// source of schema, e.g. if XSD of JSON schema data is available for the collecction.  Also
// F# could be used with any future code generators based on schema for DF collections.

type DF = 
    DataFabricProvider.DataFabricProvider<Hostname = "datafabric-tst",Port = 40001,
                                          AccessToken="7f03c2e6-79f1-472e-bda4-e6a37ebb905f", 
                                          Username= "symed", 
                                          Password="xxxxxxx">

let ctxt = DF.GetDataContext()

for d in ctxt.Databases.``Rates-ldn-uat14a``.RatesTradeQueryable.Documents -> d. ]

let sampleRateTradeInfo = 
    query { for d in ctxt .Databases.``Rates-ldn-uat14c``.RatesTradeQueryable.Documents do
            take 10
            yield d.key.id,  d.bookName }
    |> Seq.toArray

// Now get the data we'll need for the PV computations below, using a query over DF

let rateTrades = 
    query { for d in ctxt .Databases.``Rates-ldn-uat14c``.RatesTradeQueryable.Documents do
            take 100
            // Note, the book ID is not always present, so we use the bookName when not present
            let bookId = (try d.bookId with _ -> d.bookName)
            yield d.key.id, bookId, d.priceable }
    |> Seq.toArray

//------------------------------------
// Demo part 4 – F# + DF + CAF 
//
// In this section we simply compose the two above: drawing data from DF and using it with CAF


let cafTradeInfo = 
    [ for (id, book, priceable) in rateTrades do
          let tradeXml = CAF.GetTradeXML priceable.``$binary``
          yield CAF.GetTradeFromXmlOrIdm tradeXml ] 

//------------------------------------
// Demo part 5 – F# + DF + CAF + MBrace
//
// In this section we go further to compose four technologies: 
//    - Drawing data from DF in a strongly typed way
//    - Using CAF to extract trade descriptions and do pricing 
//    - Using MBrace to scale-out the use of CAF over multiple machines
//    - Using F# as the programming language

// The following is a distributed, parallel computation to run over many machines
let pvsJob2 = 
    [ for (strike, mdf) in strikes -> 
        local { 
          let results = 
               [ for (id, book, priceable) in rateTrades -> 
                    let trade = CAF.GetTradeXML priceable.``$binary``
                    CAF.PriceTradeXmlOrIdm (trade, "capMarketData") ]
          return results  }  ] 
    |> Cloud.Parallel 
    |> cluster.CreateProcess

pvsJob2.ShowInfo()

cluster.ShowWorkers()

// Wait for the results
let pvs2 = pvsJob2.Result

// Show or report the results
printfn "pvs = %A" pvs 


//------------------------------------
// Demo part 6 – F# + DF + CAF + MBrace + MBrace.Flow
//
// Finally, in this part we add a last technology: the MBrace.Flow library
// This is used for data-parallel, distributed jobs drawing data from 
// partitioned inputs. Mbrace.Flow can be thought of as a simpler, more
// powerful and more flexible version of the Hadoop or Spark map/reduce paradigm.
//
// The example below is similar to the one we have just run except that it uses
// CloudFlow, adding an aggregation step which groups and sum the PVs of the trades by book id.

let pvsJob3 = 
    rateTrades 
    |> CloudFlow.OfArray
    |> CloudFlow.sumByKey  
           (fun (_, book, _) -> book)  // group by the book
           (fun (_, _, priceable) -> // sum by the price
               let trade = (CAF.GetTradeXML priceable.``$binary``, "capMarketData")
               CAF.PriceTradeXmlOrIdm trade ) 
    |> CloudFlow.filter (fun (_, avg) -> avg <> 0.0)
    |> CloudFlow.toArray
    |> cluster.CreateProcess

printfn "running..."

let pvs3 = pvsJob3.Result

printfn "pvs = %A" pvs 


// Example output:
//
//val pvs3 : (string * double) [] =
//  [|("RS:GDSLDN-336", 0.0); ("RS:GDSLDN-611", 0.0); ("RS:SYXLDN-12388", 0.0);
//    ("RS:GDSLDN-2121", 0.0); ("ECU Portfolio", 0.0); ("RS:GDSLDN-2513", 0.0);
//    ("RS:GDSLDN-2393", 0.0); ("RS:GDSLDN-2973", 0.0);
//    ("RS:SYXLDN-12392", 410027619.8); ("RS:GDSLDN-861", 0.0);
//    ("RS:GDSLDN-885", 0.0); ("RS:GDSLDN-2205", 0.0); ("UNKNOWN", 0.0);
//    ("RS:GDSLDN-2783", 0.0); ("RS:GDSLDN-2093", 0.0); ("RS:GDSLDN-4312", 0.0);
//    ("RS:GDSLDN-2255", 0.0)|]


//------------------------------------
// Future Work: 
//   1. Add a feature to the type provider to give typed IObservable streams coming from DF.
//      Use these to do an incremental PV computation. Then fed that into an MBrace queue and do 
//      that in the cloud.


//------------------------------------
// Go home

cluster.KillAllWorkers()

