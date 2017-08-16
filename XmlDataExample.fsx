
#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
open FSharp.Data

// A basic sample

type Author = XmlProvider<"""<author name="Paul Feyerabend" born="1924" />""" (* ,InferTypesFromValues=false *) >
let sample = Author.Parse("""<author name="Karl Popper" born="1902" />""")


//sample.Nammme
sample.Born


let authors = """
  <authors topic="Philosophy of Mathematics">
    <author name="Bertrand Russell" />
    <author name="Ludwig Wittgenstein" born="1889" />
    <author name="Alfred North Whitehead" died="1947" />
  </authors> """


type Authors = XmlProvider<"data/Writers.xml">
let topic = Authors.Parse(authors)

printfn "%s" topic.Topic
for author in topic.Authors do
    printf " - %s" author.Name 
    author.Born |> Option.iter (printf " (%d)")
    printfn ""


//-----------------------------------------------------
// XML from the web


type Rss = XmlProvider<"http://tomasp.net/blog/rss.aspx">
  

let blog = Rss.GetSample()
blog.Channel.Items.[0].Link

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


