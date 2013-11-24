(*
#r "Samples.Hadoop.TypeProviders.dll"
open Samples.Hadoop

type HadoopData = HiveTypeProvider<"openfsharp.cloudapp.net",DefaultTimeout=20000,UseUnitAnnotations=true, UserName="drs1004",Password="jHadoop1">

let data = HadoopData.GetDataContext()

let analysis = 
    query { for x in data.hivesampletable do
            select x.devicemodel }
      |> Seq.toList



*)

#r "bin/Samples.Hadoop.TypeProviders.dll"
#r "bin/HadoopHiveProxyLib.dll"

open Samples.Hadoop

type HadoopData = HiveTypeProvider<"tryfsharp", UserName="drs1004",Password="hadoop">

let data = HadoopData.GetDataContext()

let analysis = 
    query { for x in data.winequalitywhite do
            select x.chlorides }
      |> Seq.toList


