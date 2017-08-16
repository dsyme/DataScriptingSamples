
#r @"packages\XPlot.Plotly\lib\net45\XPlot.Plotly.dll"

#r @"packages\Http.fs\lib\net40\HttpClient.dll"

open XPlot.Plotly

module BasicBarChart = 
    Plotly.Signin("dsyme","1cmib2v98e")

    let basicData =
        Data(
            [
                Bar(
                    x = ["giraffes"; "orangutans"; "monkeys"],
                    y = [20; 14; 23]
                )
            ]
        )

    Figure(basicData).Plot("test1")





open FSharp.Charting
open FSharp.Charting.ChartTypes

open System
open System.Drawing
let data = [ for x in 0 .. 99 -> (x,x*x) ]
let data2 = [ for x in 0 .. 99 -> (x,sin(float x / 10.0)) ]
let data3 = [ for x in 0 .. 99 -> (x,sin(float x / 10.0)) ]


Chart.Line data2
Chart.Line(data,Title="Test Title")
Chart.Line(data,Title="Test Title").WithTitle(InsideArea=false)

Chart.Line(data,Name="Test Data").WithXAxis(Enabled=false)


Chart.Line(data,Name="Test Data").WithLegend(Title="Hello")
Chart.Line(data,Name="Test Data").WithLegend(Title="Hello",Enabled=false)

Chart.Line(data,Name="Test Data").With3D()

Chart.Line(data,Name="Test Data",XTitle="hello", YTitle="goodbye")

Chart.Line(data,Name="Test Data").WithXAxis(Title="XXX")
Chart.Line(data,Name="Test Data").WithXAxis(Title="XXX",Max=10.0,Min=4.0).WithYAxis(Title="YYY",Max=100.0,Min=4.0,Log=true)

Chart.Combine [ Chart.Line(data,Name="Test Data With Long Name 1")
                Chart.Line(data2,Name="Test Data 2") ]                 
   |> fun c -> c.WithLegend(Enabled=true,Title="Hello",Docking=Docking.Left)

Chart.Combine [ Chart.Line(data,Name="Test Data 1")
                Chart.Line(data2,Name="Test Data 2") ]                 
   |> fun c -> c.WithLegend(Docking=Docking.Left, InsideArea=true)
   
Chart.Combine [ Chart.Line(data,Name="Test Data 1")
                Chart.Line(data2,Name="Test Data 2") ]                 
   |> fun c -> c.WithLegend(InsideArea=true)
   

Chart.Rows 
     [ Chart.Line(data,Title="Chart 1", Name="Test Data 1")
       Chart.Line(data2,Title="Chart 2", Name="Test Data 2") ]                 
   |> fun c -> c.WithLegend(Title="Hello",Docking=Docking.Left)

Chart.Columns
     [ Chart.Line(data,Title="Chart 1", Name="Test Data 1")
       Chart.Line(data2,Title="Chart 2", Name="Test Data 2")]                 
   |> fun c -> c.WithLegend(Title="Hello",Docking=Docking.Left)

Chart.Rows 
     [ Chart.Line(data,Title="Chart 1", Name="Test Data 1").WithTitle(InsideArea=false).WithLegend(Docking=Docking.Left)
       Chart.Line(data,Title="Chart 2", Name="Test Data 2").WithTitle(InsideArea=false).WithLegend(Docking=Docking.Right) ] 
                
   |> fun c -> c.WithLegend(Title="Hello") // TODO: this docking left doesn't work

Chart.Combine [ Chart.Line(data,Name="Test Data 1")
                Chart.Line(data2,Name="Test Data 2") ]                 
   |> fun c -> c.WithLegend(Title="Hello",Docking=Docking.Bottom)

Chart.Line(data,Name="Test Data")
Chart.Line(data,Name="Test Data").WithLegend(Enabled=false)
Chart.Line(data,Name="Test Data").WithLegend(InsideArea=true)
Chart.Line(data,Name="Test Data").WithLegend(InsideArea=false)
Chart.Line(data).WithLegend().CopyAsBitmap()

Chart.Line(data)



Chart.Line(data,Name="Test Data").WithLegend(InsideArea=false)


Chart.Area(data)
Chart.Bar(data)

let rnd = new System.Random()
let rand() = rnd.NextDouble()

let pointsWithSizes1 = [ for i in 0 .. 30 -> (rand() * 10.0, rand() * 10.0, rand() / 100.0) ]
let pointsWithSizes2 = [ for i in 0 .. 10 -> (rand() * 10.0, rand() * 10.0, rand() / 100.0) ]

Chart.Spline(data)

Chart.Bubble(pointsWithSizes1)
Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Star10)
Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Diamond)
Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Cross,Color=Color.Red)
// TODO: these don't seem to change the size
Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Cross,Color=Color.Red,MaxPixelPointWidth=3)
Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Cross,Size=3)
Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Cross,PointWidth=0.1)
Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Cross,PixelPointWidth=3)

Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Circle)
Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Square)
Chart.Bubble(pointsWithSizes1).WithMarkers(Style=MarkerStyle.Star6)
Chart.Combine [ Chart.Bubble(pointsWithSizes1,UseSizeForLabel=true) .WithMarkers(Style=MarkerStyle.Circle)
                Chart.Bubble(pointsWithSizes2).WithMarkers(Style=MarkerStyle.Star10) ]

let timeHighLowOpenClose = [ for i in 0 .. 10 -> let mid = rand() * 10.0 in (i, mid + 0.5, mid - 0.5, mid + 0.25, mid - 0.25) ]
Chart.Candlestick  timeHighLowOpenClose

Chart.Column data
Chart.Pie(Name="Pie", data=[ for i in 0 .. 10 -> i, i*i ])
Chart.Doughnut(data=[ for i in 0 .. 10 -> i, i*i ])
Chart.FastPoint [ for x in 1 .. 10000 -> (rand(), rand()) ]

Chart.Polar ([ for x in 1 .. 100 -> (360.0*rand(), rand()) ] |> Seq.sortBy fst)
Chart.Pyramid ([ for x in 1 .. 100 -> (360.0*rand(), rand()) ] |> Seq.sortBy fst)
Chart.Radar ([ for x in 1 .. 100 -> (360.0*rand(), rand()) ] |> Seq.sortBy fst)
Chart.Range ([ for x in 1.0 .. 10.0 -> (x, x + rand(), x-rand()) ])
Chart.RangeBar ([ for x in 1.0 .. 10.0 -> (x, x + rand(), x-rand()) ])
Chart.RangeColumn ([ for x in 1.0 .. 10.0 -> (x, x + rand(), x-rand()) ])
Chart.SplineArea ([ for x in 1.0 .. 10.0 -> (x, x + rand()) ])
Chart.SplineRange ([ for x in 1.0 .. 10.0 -> (x, x + rand(), x - rand()) ])
//Chart.StackedBar ([ for x in 1.0 .. 10.0 -> (x, x + rand(), x - rand()) ])

Chart.Line(data,Name="SomeData").WithDataPointLabels(PointToolTip="Hello, I am #SERIESNAME") 



let form = new System.Windows.Forms.Form(Visible=true,TopMost=true)

let evData = form.MouseMove |> Event.map (fun e -> e.Y) |> Event.sampled 30 |> Event.windowTimeInterval 3000
let evData2 = Event.clock 20 |> Event.map (fun x -> (x, 100.0 + 200.0 * sin (float (x.Ticks / 2000000L)))) |> Event.windowTimeInterval 3000 

LiveChart.Line(evData,Name="MouseMove")
LiveChart.Line(evData2,Name="Clock")

Chart.Combine 
  [ LiveChart.Line(evData,Name="MouseMove")
    LiveChart.Line(evData2,Name="Wave") ]

Chart.Bubble(pointsWithSizes1,UseSizeForLabel=true)
