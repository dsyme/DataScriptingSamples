
#load "extlib/EventEx-0.1.fsx"
#load "packages/FSharp.Charting/FSharp.Charting.fsx"
#load "vizlib/show.fsx"
#r "System.Xml.Linq"

[ for i in 0 .. 100 -> (i, i*i) ] |> showGrid

// ----------------------------------------------------------------------------
// Reference the FSharp.Data provider 


#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data
open FSharp.Charting
open FSharp.Charting.ChartTypes

//------------------------------------------------
// Types from JSON data 


type Simple = JsonProvider<""" [{ "name":"John", "age":94 }] """>

let simple = Simple.Parse(""" [{ "name":"Tomas", "age":28 }, { "name":"Simon", "age": 21 }] """)

simple |> showGrid


//------------------------------------------------
// Types from JSON data on the web

type GitHub = JsonProvider<"https://api.github.com/repos/dotnet/coreclr/issues">

let mostCommentedIssues = 
    GitHub.GetSamples()
    |> Seq.filter (fun issue -> issue.State = "open")
    |> Seq.sortByDescending (fun issue -> issue.Comments) 
    |> Seq.toArray

mostCommentedIssues |> showGrid

[ for i in mostCommentedIssues  -> i.Title, i.Comments ] |> showGrid

// Generalize to any repo

let getMostCommentedIssues repo = 
    GitHub.Load("https://api.github.com/repos/" + repo + "/issues")
    |> Seq.filter (fun issue -> issue.State = "open")
    |> Seq.sortByDescending (fun issue -> issue.Comments) 
    |> Seq.toArray

getMostCommentedIssues "Microsoft/visualfsharp" |> showGrid

getMostCommentedIssues "fsharp/fsharp" |> showGrid

getMostCommentedIssues "fsharp/FSharp.Compiler.Service" |> showGrid






// ----------------------------------------------------------------------------
// Types from Data: XML from the web


type Atom = XmlProvider<"http://www.theregister.co.uk/software/headlines.atom">
    

let theRegister = Atom.Load("http://www.theregister.co.uk/software/headlines.atom")
[ for i in theRegister.Entries -> i.Title.Value, i.Updated ]  |> showGrid

let getAtomTitles (url:string) = 
    let feed = Atom.Load(url)
    [ for i in feed.Entries -> i.Title.Value, i.Updated ]  

getAtomTitles "http://www.theregister.co.uk/software/headlines.atom" |> showGrid
getAtomTitles "http://www.theregister.co.uk/hardware/headlines.atom" |> showGrid


//-----------------------------------------------------
// Transforming XML 


[<Literal>]
let customersXmlSample = """
  <Customers>
    <Customer name="ACME">
      <Order Number="A012345">
        <OrderLine Item="widget" Quantity="1"/>
      </Order>
      <Order Number="A012346">
        <OrderLine Item="trinket" Quantity="2"/>
      </Order>
    </Customer>
    <Customer name="Southwind">
      <Order Number="A012347">
        <OrderLine Item="skyhook" Quantity="3"/>
        <OrderLine Item="gizmo" Quantity="4"/>
      </Order>
    </Customer>
  </Customers>"""

// Transform it to this
[<Literal>]
let orderLinesXmlSample = """
  <OrderLines>
    <OrderLine Customer="ACME" Order="A012345" Item="widget" Quantity="1"/>
    <OrderLine Customer="ACME" Order="A012346" Item="trinket" Quantity="2"/>
    <OrderLine Customer="Southwind" Order="A012347" Item="skyhook" Quantity="3"/>
    <OrderLine Customer="Southwind" Order="A012347" Item="gizmo" Quantity="4"/>
  </OrderLines>"""

type InputXml = XmlProvider<customersXmlSample>
type OutputXml = XmlProvider<orderLinesXmlSample>

let inputs = InputXml.GetSample().Customers

let orders = 
  [ for customer in inputs do
      for order in customer.Orders do
        for line in order.OrderLines do
          yield (customer.Name,order.Number,line.Item,line.Quantity) ]

let orderLines = 
   OutputXml.OrderLines 
      [| for (name, number, item, quantity) in orders do
            yield OutputXml.OrderLine(name, number, item, quantity) |]

orderLines.XElement.ToString()












//----------------------------------------


let data = FSharp.Data.WorldBankData.GetDataContext()


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
             




