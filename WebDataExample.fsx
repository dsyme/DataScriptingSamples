
#load "extlib/EventEx-0.1.fsx"
#load "packages/FSharp.Charting/FSharp.Charting.fsx"
#load "vizlib/show.fsx"

//-----------------------------------------------
// First some data scripting





let tableOfSquares = 
    [ for i in 0 .. 99 -> (i, i*i)  ] 


tableOfSquares |> showGrid

//-----------------------------------------------
// Now some charting

open FSharp.Charting
open FSharp.Charting.ChartTypes

Chart.Line [ for i in 0 .. 99 -> (i, i*i) ]
Chart.Pie [ for i in 0 .. 99 -> (i, i*i) ]
Chart.ErrorBar [ for i in 0.0 .. 3.1 .. 100.0 -> (i, i*i, i*i*0.90, i*i*1.10) ]


let rnd = System.Random()
let rand() = rnd.NextDouble()

Chart.Point [ for i in 0 .. 10000 -> (rand(),rand()*rand()) ]
    
let form = new System.Windows.Forms.Form(Visible=true,TopMost=true)

form.MouseMove
   |> Event.map (fun e -> e.X, 500-e.Y) 
   |> LiveChart.LineIncremental



form.MouseMove
   |> Event.map (fun e -> System.DateTime.Now, 500-e.Y) 
   |> LiveChart.LineIncremental

form.MouseMove 
    |> Event.map (fun e -> e.Y) 
    |> Event.sampled 30 
    |> Event.windowTimeInterval 3000
    |> LiveChart.Line
 

// ----------------------------------------------------------------------------
// World Bank data

#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

let data = FSharp.Data.WorldBankData.GetDataContext()


//----------------------------------------


let countries = 
   [ data.Countries.``El Salvador``
     data.Countries.China 
     data.Countries.Malaysia
     data.Countries.Singapore
     data.Countries.Germany
     data.Countries.Greece
     data.Countries.``United States``
     data.Countries.India
     data.Countries.Afghanistan
     data.Countries.``Yemen, Rep.``
     data.Countries.Bangladesh ]



/// Chart the populations, un-normalized
Chart.Combine([ for c in countries -> Chart.Line (c.Indicators.``Tax revenue (% of GDP)``, Name=c.Name) ])
     .WithTitle("Tax, 1960-2012")
     .WithLegend()




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
             



