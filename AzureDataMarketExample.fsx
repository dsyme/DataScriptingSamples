#r "System.Data.Services.Client"
#r "bin/Samples.WindowsAzure.Marketplace.dll"

open Samples.WindowsAzure.Marketplace

//type T = Samples.WindowsAzure.Marketplace.AllData.Data_Quality_Services.IP_Check
//let ctxt = T.GetDataContext()


type T2 = MyData.Communications.Microsoft_Translator.ODataService

let ctxt = T2.GetDataContext()

ctxt.Detect("Alle meine Entchen")
ctxt.Translate("Alle meine Entchen", "en", "de")
ctxt.Translate("Alle meine Entchen", "fr", "de")
ctxt.Translate("Alle meine Entchen", "es", "de")




type IC = Samples.WindowsAzure.Marketplace.AllData.Science_and_Statistics.Industrial_Commodity_Statistics_Database.ODataService


let ctxt2 = IC.GetDataContext()

ctxt2.DataSeries


//type Service = Samples.WindowsAzure.Marketplace.AllData.Government.Local_Authority_Average_Rents.ODataService 

//let ctxt = Service.GetDataContext()




//ctxt.AddToLocalAuthorityRentsBorough(Service.ServiceTypes.LocalAuthorityRentsBorough.CreateLocalAuthorityRentsBorough("Cambridge"))



(*
#r "System.Data.Services.Client"
#r "../TypeProviders/Debug/net40/Samples.WindowsAzure.Marketplace.dll"
 *)














//type Service = Samples.WindowsAzure.Marketplace.AllData.Government.AgCube.ODataService 
//let ctxt = Service.GetDataContext()

//type Service2 = Samples.WindowsAzure.Marketplace.AllData.Government.London_Borough_Profiles.ODataService 
//let ctxt2 = Service2.GetDataContext()




(*

type T2 = Samples.WindowsAzure.Marketplace.MyData.Communications.Microsoft_Translator.ODataService

let ctxt2 = T2.GetDataContext()

ctxt2.GetLanguagesForTranslation()
ctxt2.Translate("Hello there", "de", "en")
ctxt2.Translate("Ich habe keine Ahnung, was du meinst", "en", "de")


*)
//----------------------------------------------

(*
type T = Samples.WindowsAzure.Marketplace.MyData.Business_and_Finance.World_Development_Indicators.ODataService

let ctxtWB = T.GetDataContext()

ctxtWB.GetCountries("en")
ctxtWB.GetIndicators("en")
ctxtWB.GetData("en", "AUS", "SP.POP.TOTL")


ctxtWB.GetCountries("en") |> Seq.toList

*)

//----------------------------------------------
(*

type T2 = Samples.WindowsAzure.Marketplace.MyData.Communications.Microsoft_Translator.ODataService

let ctxt2 = T2.GetDataContext()

ctxt2.GetLanguagesForTranslation()
ctxt2.Translate("Hello there", "de", "en")
ctxt2.Translate("Ich habe keine Ahnung, was du meinst", "en", "de")

//----------------------------------------------

type T3 = Samples.WindowsAzure.Marketplace.MyData.Reference.DateStream.ODataService
let ctxt3 = T3.GetDataContext()

let us = query { for x in ctxt3.BasicCalendarUS do where (x.YearKey = 2012); where (x.DateKey = System.DateTime(2012,4,9)); select x }
us |> Seq.toList 


//----------------------------------------------

type T4 = Samples.WindowsAzure.Marketplace.AllData.Business_and_Finance.CCH_CorpSystem_Sales_Tax_Rates.ODataService
let ctxt4 = T4.GetDataContext()

//----------------------------------------------

type T5 = Samples.WindowsAzure.Marketplace.AllData.Business_and_Finance.China_Small_and_Medium_Enterprises_Management_and_Operation_KPI_Data.ODataService
let ctxt5 = T5.GetDataContext()

//----------------------------------------------

type T6 = Samples.WindowsAzure.Marketplace.AllData.Business_and_Finance.DandB_Business_Lookup.ODataService
let ctxt6 = T6.GetDataContext()

//----------------------------------------------

type T7 = Samples.WindowsAzure.Marketplace.AllData.Business_and_Finance.DisclosingJapan.ODataService
let ctxt7 = T7.GetDataContext()

//----------------------------------------------

type T8 = Samples.WindowsAzure.Marketplace.AllData.Business_and_Finance.ParcelAtlas_REPORTS.ODataService
let ctxt8 = T8.GetDataContext()

//----------------------------------------------

type T9 = Samples.WindowsAzure.Marketplace.AllData.Business_and_Finance.``The_Stock_Sonar_-_Sentiment_Service_of_US_Stocks``.ODataService
let ctxt9 = T9.GetDataContext()

//----------------------------------------------

type T10 = Samples.WindowsAzure.Marketplace.AllData.Business_and_Finance.Phone_Number_Validation.ODataService
let ctxt10 = T10.GetDataContext()


//----------------------------------------------

type T11 = Samples.WindowsAzure.Marketplace.AllData.Business_and_Finance.``Eurex_-_Daily_Aggregated_Market_Data``.ODataService
let ctxt11 = T11.GetDataContext()


//----------------------------------------------

type T12 = Samples.WindowsAzure.Marketplace.AllData.Business_and_Finance.Datawire.ODataService
let ctxt12 = T12.GetDataContext()


//----------------------------------------------

type T13 = Samples.WindowsAzure.Marketplace.AllData.Banking.MavaIQ_Currencies.ODataService
let ctxt13 = T13.GetDataContext()

//----------------------------------------------

type T14 = Samples.WindowsAzure.Marketplace.AllData.Banking.Federal_Reserve_Bank_Financial_Data.ODataService
let ctxt14 = T14.GetDataContext()
*)

//----------------------------------------------

(*
type T15 = Samples.WindowsAzure.Marketplace.AllData.Data_Quality_Services.Address_Check.ODataService
let ctxt15 = T15.GetDataContext()
*)


