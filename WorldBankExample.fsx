
// ----------------------------------------------------------------------------
// Load the charting library

#load "packages/FSharp.Charting/FSharp.Charting.fsx"

open FSharp.Charting
open FSharp.Charting.ChartTypes
 
// ----------------------------------------------------------------------------
// Reference the provider for the World Bank and explore the data

#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"


let data = FSharp.Data.WorldBankData.GetDataContext()

let countries = 
   [ data.Countries.``El Salvador``
     data.Countries.China 
     data.Countries.Malaysia
     data.Countries.Singapore
     data.Countries.Germany
     data.Countries.``United Kingdom``
     data.Countries.``United States``
     data.Countries.India
     data.Countries.Afghanistan
     data.Countries.``Yemen, Rep.``
     data.Countries.Bangladesh ]



/// Chart the populations, un-normalized
Chart.Combine([ for c in countries -> Chart.Line (c.Indicators.``GDP (current US$)``, Name=c.Name) ])
     .WithTitle("GDP (current US$), 1960-2012")
     .WithLegend(Enabled=true)




Chart.Pie
   [ for c in countries -> c.Name,  c.Indicators.``Population, total``.[2001] ]


let countries1 = 
  [ data.Countries.India; data.Countries.Uganda; data.Countries.Ghana;
    data.Countries.``Burkina Faso``; data.Countries.Niger; data.Countries.Malawi
    data.Countries.Afghanistan; data.Countries.Cambodia; data.Countries.Bangladesh
  ]

let pointdata = 
    [ for country in countries1 ->
          let y = country.Indicators.``Adolescent fertility rate (births per 1,000 women ages 15-19)``.[2005]
          let x = country.Indicators.``Primary completion rate, female (% of relevant age group)``.[2005]
          x,y ]
                 

Chart.Point(pointdata)
     .WithXAxis(Title="Adolescent fertility rate (births per 1,000 women ages 15-19)")
     .WithYAxis(Title="Primary completion rate, female (% of relevant age group)")
     .WithMarkers(Size=40,Style=MarkerStyle.Diamond)



data.Countries.Australia.Indicators.``Population, total``

// ----------------------------------------------------------------------------
// Work with time series data

#load "packages/Deedle/Deedle.fsx"


data.Countries.``United States``.Indicators.``Health expenditure, total (% of GDP)``
|> Chart.Line


let countries2 = 
  [ data.Countries.``United States``; data.Countries.Switzerland
    data.Countries.Denmark; data.Countries.``United Kingdom``;
    data.Countries.``Czech Republic`` ]

Chart.Combine([ for country in countries2 ->
                    let data = country.Indicators.``Health expenditure per capita (current US$)``
                    Chart.Line(data, Name=country.Name) ])
     .WithTitle("Health expenditure per capita (current US$)")
     .WithLegend(InsideArea=false)


Chart.Combine([ for country in countries2 ->
                    let data = country.Indicators.``Mortality rate, infant (per 1,000 live births)``
                    Chart.Line(data, Name=country.Name) ])
     .WithTitle("Mortality rate, infant (per 1,000 live births)")
     .WithXAxis(Max=2011.0 (* , TickMarks=[1960..3..2010] *) )
             




















let (++) xs ys = Seq.append xs ys

let popAt year =
 ((query { for c in data.Countries  do 
           let pop = c.Indicators.``Population, total``.TryGetValueAt(year) |> (fun c -> defaultArg c 0.0)
           where (pop > 15000000.0) 
           select (c.Name, pop) }
   ++
   [ ("Other", 
      query { for c in data.Countries  do 
              let pop = c.Indicators.``Population, total``.TryGetValueAt(year) |> (fun c -> defaultArg c 0.0)
              where (pop < 15000000.0) 
              sumBy (pop) }) ])
   
   |> Seq.sortBy snd)

#load "extlib/AsyncSeq-0.1.fsx"
open FSharp.Charting.ChartTypes

open Samples.FSharp.AsyncSeq
asyncSeq { for year in 1960..10..2009 do yield popAt year; do! Async.Sleep 1000 } 
   |> AsyncSeq.StartAsEvent
   |> LiveChart.Pie
Chart.Pie(popAt 2004,Name="Population")


/// Normalize the time series using the value at the given key as the 1.0 value
let normalize key xs = 
    let firstValue = xs |> Seq.find (fun (y,v) -> y = key) |> snd
    xs |> Seq.map (fun (y,v) -> (y, float v / firstValue) )

/// Test it
data.Countries.Australia.Indicators.``Population, total`` |> normalize 1960 
data.Countries.Australia.Indicators.``Population, total`` |> normalize 1960 |> Chart.Line
data.Countries.Australia.Indicators.``Population, total`` |> normalize 1960 |> Chart.Line

//data.GetCountry("AUS")._GetIndicator("SP.POP.TOTL").GetValueAtOrZero(2001)

[ for c in data.Countries -> c.Code ]
[ for c in data.Regions -> c.Name ]

/// Chart the populations, normalized
Chart.Combine 
    [ for c in countries -> 
        let data = c.Indicators.``Population, total`` |> normalize 1960
        Chart.Line(data, Name=c.Name)]
    |> fun c -> c.WithTitle("Population, Normalized").WithLegend(InsideArea=false)

Chart.Combine 
    [ for c in countries ->
        let data = c.Indicators.``International migrant stock (% of population)``
        Chart.Line (data, Name=c.Name) ]
    |> fun c -> c.WithTitle("International migrants")




// ----------------------------------------------------------------------------
// How are we doing on debt?

data.Countries.Greece.Indicators.``Central government debt, total (% of GDP)``
|> Chart.Line


// Plot debt of different countries in a single chart using nicer chart style

let countries4 = 
  [ data.Countries.Greece; data.Countries.Ireland; 
    data.Countries.Denmark; data.Countries.``United Kingdom``;
    data.Countries.``Czech Republic`` ]

Chart.Combine
  [ for country in countries4 ->
      let data = country.Indicators.``Central government debt, total (% of GDP)``
      Chart.Line(data, Name=country.Name) ]
|> fun c -> c.WithTitle("Central government debt, total") //  .WithLegend(Docking = Docking.Left)


// ----------------------------------------------------------------------------
// University enrollment

open System.Drawing
    
/// Calculate average university enrollment for EU
/// (This is slow because it needs to download info for every EU country)
let avgEU =
    [ for c in data.Regions.``European Union``.Countries do
        yield! c.Indicators.``School enrollment, tertiary (gross), gender parity index (GPI)`` ]
    |> Seq.groupBy fst
    |> Seq.map (fun (y, v) -> y, Seq.averageBy snd v)
    |> Array.ofSeq
    |> Array.sortBy fst

/// Calculate average university enrollment for OECD
/// (This is slow because it needs to download info for every OECD country)
let avgOECD =
    [ for c in data.Regions.``OECD members``.Countries do
        yield! c.Indicators.``School enrollment, tertiary (gross), gender parity index (GPI)`` ]
    |> Seq.groupBy fst
    |> Seq.map (fun (y, v) -> y, Seq.averageBy snd v)
    |> Array.ofSeq
    |> Array.sortBy fst

// Generate nice line chart combining CZ, EU and OECD enrollment
Chart.Combine
  [ yield Chart.Line(avgEU, Name="EU", Color=Color.Blue)
    yield Chart.Line(avgOECD, Name="OECD", Color=Color.Goldenrod)
    let cze = data.Countries.``Czech Republic``
    yield Chart.Line(data=cze.Indicators.``School enrollment, tertiary (gross), gender parity index (GPI)``,Name="CZ",Color=Color.DarkRed)  ]
|> fun c -> c.WithLegend(Docking = Docking.Left)





