
// This sample demonstrates the use of the F# Hadoop/Hive provider from Visual Studio.
//    - The identical programming experience is available in the browser

#r "bin/Samples.Hadoop.TypeProviders.dll"
#r "bin/HadoopHiveProxyLib.dll"

#load "extlib/QueryEx-0.1.fsx"
#load "credentials/HadoopCredentials.fsx"

open System
open Microsoft.FSharp.Linq
open Microsoft.FSharp.Linq.NullableOperators
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Samples.Hadoop


type HadoopData = HiveTypeProvider<"tryfsharp",Port=10000,UseUnitAnnotations=true>

let data = HadoopData.GetDataContext()


let testQuery1 = 
    query { for x in data.abalone do
            select x }



module AbaloneCatchAnalysis = 

    /// What's the average shell weight of the Abalone in our data set?
    let averageShellWeightOfAbalone = 
        query { for x in data.abalone do
                averageBy x.shellweight }

module MarketingCampaignAnalysis = 

        /// What's the average duration of a marketing call?
        let averageDuration = 
            query { for x in data.bankmarketing  do 
                    averageBy (float x.duration) }

        /// What's the average age for a successful marketing call?
        let averageAgeForPeopleWhoDidBuy = 
            query { for a in data.bankmarketing  do 
                    where (a.y = "yes") 
                    averageBy (float a.age)  }
 
        /// What's the average age for an unsuccessful marketing call?
        let averageAgeForPeopleWhoDidNotBuy = 
            query { for a in data.bankmarketing  do 
                    where (a.y = "no") 
                    averageBy (float a.age)  }

        /// What's the average duration for an unsuccessful marketing call?
        let averageDurationForPeopleWhoDidNotBuy = 
            query { for a in data.bankmarketing  do 
                    where (a.y = "no")
                    averageBy (float a.duration) }

        /// What's the average duration for an unsuccessful marketing call?
        let averageDurationForPeopleWhoDidBuy = 
            query { for a in data.bankmarketing  do 
                    where (a.y = "yes")
                    averageBy (float a.duration) }

        /// What's the average age for all categories of marketing call?
        let marketingAnalysis2 = 
            query { for a in data.bankmarketing  do 
                    groupBy a.y into group
                    let avAge = query { for a in group do averageBy (float a.age) }
                    select (group.Key, avAge) }
            |> Seq.toList
 
        /// What's the average (duration,age,balance,marital) for all categories of marketing call?
        let marketingAnalysis3 = 
            query { for a in data.bankmarketing  do 
                    groupBy a.y into group
                    let avDuration = query { for a in group do averageBy (float a.duration) }
                    let avAge = query { for a in group do averageBy (float a.age) }
                    let avBalance = query { for a in group do averageBy a.balance }
                    let avSingle = query { for a in group do averageBy (if a.marital = "single" then 1.0 else 0.0) }
                    let count = query { for a in group do count }
                    select (group.Key, (avDuration,avAge,avBalance,avSingle,count)) }
            |> Seq.toList
 

//type Hdfs =  Samples.Hadoop.HdfsProvider.HdfsTyped<Host="tryfsharp">

//let hdfs = Hdfs.Data.abalone.data.Head 10


(*

 //---------------------------------------------------------------------------


 /// What's the biggest Abalone in our data set?
let biggestAbalone = 
    HadoopData.abalone |> Query.maxBy (fun x -> x.height)


let averageHeight = 
    query { for x in HadoopData.abalone  do 
            averageBy x.height }

let males = 
   let maleCount = 
       query { for x in HadoopData.abalone  do 
               where (x.sex = "M") 
               count } 

   let totalCount =  
       query { for x in HadoopData.abalone  do 
               count } 

   float maleCount / float totalCount




let weightInfo = 
   let maleAvWeight = HadoopData.abalone |> Query.where (fun x -> x.sex = "M") |> Query.averageBy (fun x -> x.wholeweight)
   let femaleAvWeight = HadoopData.abalone |> Query.where (fun x -> x.sex = "F") |> Query.averageBy (fun x -> x.wholeweight)
   maleAvWeight, femaleAvWeight


let weightInfo2 = 
   query { for x in HadoopData.abalone do
           groupBy x.sex into group
           let av = query { for g in group do averageBy g.wholeweight }
           select (group.Key, av) }
   |> Query.toList

let education = 
   let smartLadiesCount = 
       query { for x in HadoopData.bankmarketing do 
               where (x.marital = "single") 
               where (x.education = "tertiary")
               count } 

   let smartLadiesCount = 
       query { for x in HadoopData.bankmarketing do 
               where (x.marital = "single") 
               where (x.loan = "yes")
               count } 

   let ladiesCount = 
       query { for x in HadoopData.bankmarketing do 
               where (x.marital = "single") 
               where (x.loan = "yes")
               count } 

   let totalCount =  
       query { for x in HadoopData.abalone  do count } 

   float ladiesCount / float totalCount


let averageHeightOfAbalone = 
    HadoopData.abalone 
        |> Query.averageBy (fun x -> x.height)

let smallestAbaloneByLength = 
    HadoopData.abalone |> Query.sortBy (fun x -> x.length) |> Query.head

let biggestAbsloneByLength = 
    HadoopData.abalone |> Query.sortBy (fun x -> x.length) |> Query.head

let lengthOfBiggestAbslone2 = 
    HadoopData.abalone |> Query.maxBy (fun x -> x.length) 

*)

//T2.
    
//for x in T2.abalone do x.
//for x in T2.bankmarketing do x.a


//T2.abalone |> Seq.head |> fun y -> y.
//query { for i in T2.abalone do         
//        select (i.diameter, i.rings) }   

//T2.carevaluation |> Seq.head |> fun y -> y.maint











//T2.bankmarketing |> Seq.head |> fun z -> z.

        

//type T9 = HiveProvider.HiveTyped<"openfsharp">

//
//type T = HiveProvider.HiveTyped<>

//
//
//
//let data = T.iris
//T.iris.First()
//
//
//
//query { for i in T.iris do 
//        where (i.sepal_width ?>= 3.0)
//        select (i.petal_length, i.sepal_length) }
//
//// Example - average of two columns
//let data2 = 
//    query { let g = query { for i in T.iris do 
//                            where (i.sepal_width ?>= 3.0) }
//            let avg1 = g.Average(fun (i: T.DataTypes.iris) -> i.petal_length)
//            let avg2 = g.Average(fun (i: T.DataTypes.iris) -> i.sepal_length)
//            select (avg1, avg2) }
//
//// Example - average of two columns, using no LINQ operators
//let data3 = 
//    query { let g = query { for i in T.iris do 
//                            where (i.sepal_width ?>= 3.0) }
//            let avg1 = query { for i in g do averageByNullable i.petal_length }
//            let avg2 = query { for i in g do averageByNullable i.sepal_length }
//            select (avg1, avg2) }
//
//
//
//
//
//
//
//
//
//
//
//// HiveQuery("select petal_length,petal_count from iris where sepal_width >= 3 limit 200")
//
////query { for i in T.iris do 
////        where (i.sepal_width ?>= 3.0)
////        select (i.petal_length, i.sepal_length) }
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//T.iris.Where(fun (i:T.DataTypes.iris) -> i.petal_length ?>= 3.0 )
//
//
//
//
//
//
//
//
//
//
//
//
//T.iris
//T.iris.Select(fun i -> 3.0 )
//T.iris.Select(fun (i:T.DataTypes.iris) -> i.petal_width) |> Seq.toList
//T.iris.Where(fun (i:T.DataTypes.iris) -> i.petal_length ?>= 3.0 )
//T.iris.Where(fun (i:T.DataTypes.iris) -> i.petal_width ?>= 1.4 )
//
//
//// Connectivity option 1: assume an ODBC Data Source has been registered for 
//// the hive connection????
//// type T = HiveTyped<OdbcDataSourceName="LocalHiveServer">
//
//// Connectivity option 2: give a full ODBC connection string
//// type T = HiveTyped<OdbcConnectionString="Provider=MSDASQL.1;Persist Security Info=False;DSN=LocalHiveServer">
//
//// Connectivity option 2: give a host/port (except we don't know how to map to a ODBC connection string)
////   + later options - "UseFramedPacketCommunication=true"
////   + later options - "UserName=foo"
////   + later options - "Password=true"
//// type T = HiveTyped<Host="myserver",Port=10000>
//
//
//
//
//
//
//(*
//    query { for i in T_iris do 
//            where (i.sepal_width ?>= 3.0) 
//            select (i.petal_length, i.sepal_length) }
//
//*)
//(*
//
//
//type IrisElement = 
//  { sepal_length : System.Nullable<float>
//    sepal_width : System.Nullable<float>
//    petal_length : System.Nullable<float>
//    petal_width : System.Nullable<float> 
//    ``class`` : string }
//
//let T_iris : System.Linq.IQueryable<IrisElement> = failwith ""
//
//// Example - simple filter/select
//let data1 = 
//    query { for i in T_iris do 
//            where (i.sepal_width ?>= 3.0) 
//            select (i.petal_length, i.sepal_length) }
//
//// Example - average of two columns
//let data2 = 
//    query { let g = query { for i in T_iris do 
//                            where (i.sepal_width ?>= 3.0) }
//            let avg1 = g.Average(fun i -> i.petal_length)
//            let avg2 = g.Average(fun i -> i.sepal_length)
//            select (avg1, avg2) }
//
//// Example - average of two columns, using no LINQ operators
//let data3 = 
//    query { let g = query { for i in T_iris do 
//                            where (i.sepal_width ?>= 3.0) }
//            let avg1 = query { for i in g do averageByNullable i.petal_length }
//            let avg2 = query { for i in g do averageByNullable i.sepal_length }
//            select (avg1, avg2) }
//
//
////-----
//
//(*
//each : Column<T> -> T
//avg : Column<T> -> T
//select : T -> unit
//sortBy : T -> unit
//
//let data4 = 
//    query2 { from T.iris
//             where (each T.iris.sepal_width ?>= 3.0) 
//             select (T.iris.petal_length, T.iris.sepal_length) }
//
//let data4b = 
//    query2 { from T.iris
//             where (each T.iris.sepal_width ?>= 3.0) 
//             sortBy (each T.iris.sepal_width) 
//             select (avg T.iris.petal_length, avg T.iris.sepal_length) }
//*)
//
//
//              //where (i.sepal_length = 3) 
//              //sortByNullable i.petal_length
//              //select (g.Count())// note, huge list of functions here
//              //take 200 
//
//let result1 = T_iris.Where(fun i -> (i.sepal_length ?= 3.0)).Take(200).Average(fun i -> i.petal_length)
//let result2 = T_iris.Where(fun i -> (i.sepal_length ?= 3.0)).Take(200).Average(fun i -> i.petal_length)
//
//
//
//
//T_iris.Where(fun i -> 
//
//count: string -> string
//select: string -> string
// 
//select (count(T.petal_length) (T.sepal_length === Q "3")
//
//
//
// //) .Take(200).Average(fun i -> i.petal_length)
//
//
// //--> select avg(petal_length),avg(sepal_length) from iris where sepal_width >= 3 order by sepal_width, sepal_length limit 200
//*)
