//------------------------------------------------
// JSON data type provider samples


#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions

//------------------------------------------------
// Basic JSON data 

let animals = 
    JsonValue.Parse
        """
        { "dogs": 
            [ { "category": "Companion dogs",
                "name": "Chihuahua" },
              { "category": "Hounds",
                "name": "Foxhound" } ] }
         """

let dogs = 
    [ for dog in animals?dogs  -> dog?name ]

//--------------------------------------------

type Simple = JsonProvider<""" { "name":"John", "age":94 } """>





let simple = Simple.Parse(""" { "name":"Tomas", "age":4 } """)





simple.Age
simple.Name

//--------------------------------
type Customers = JsonProvider<"""
  { "customers" : 
      [ { "name" : "ACME",
          "orders" : 
             [ { "number" : "A012345", 
                 "item" : "widget", 
                 "quantity" : 1 } ] } ] }
""">

let customers = Customers.Parse """
  { "customers" : 
      [ { "name" : "Apple Store",
          "orders" : 
              [ { "number" : "B73284", 
                  "item" : "iphone5", 
                  "quantity" : 18373 },
                { "number" : "B73238", 
                  "item" : "iphone6", 
                  "quantity" : 3736 } ] },
        { "name" : "Samsung Shop",
          "orders" : 
              [ { "number" : "N36214", 
                  "item" : "nexus7", 
                  "quantity" : 18373 } ] } ] }
"""

[ for c in customers.Customers -> c.Name ]

let newOrder = Customers.Order(number = "N36214", item = "nexus7", quantity = 1636)

let newCustomer = Customers.Customer(name = "FabPhone", orders = [| newOrder|])

let jsonText = newCustomer.JsonValue.ToString()

//-------------------

type Person = JsonProvider<"""
{
  "firstName": "John",
  "lastName": "Smith",
  "age": 25,
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    }
  ]
}""">

Person.Parse("""
{
  "firstName": "John",
  "lastName": "Smith",
  "age": 25,
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    }
  ]
}""")


//------------------------------------------------
// Types from data on the web

type GitHub = JsonProvider<"https://api.github.com/repos/dotnet/coreclr/issues">

let topRecentlyUpdatedIssues = 
    GitHub.GetSamples()
    |> Seq.filter (fun issue -> issue.State = "open")
    |> Seq.sortBy (fun issue -> System.DateTime.Now - issue.UpdatedAt)
    |> Seq.truncate 5

for issue in topRecentlyUpdatedIssues do
    printfn "#%d %s" issue.Number issue.Title
    
// -----------------------------------------------
// Creating data

[<Literal>]
let issueSample = """
{
  "title": "Found a bug",
  "body": "I'm having a problem with this.",
  "assignee": "octocat",
  "milestone": 1,
  "labels": [
    "Label1",
    "Label2"
  ]
}
"""

type GitHubIssue = JsonProvider<issueSample, RootName="issue">

let newIssue = GitHubIssue.Issue("Test issue",
                                 "This is a test issue created in F# Data documentation", 
                                 assignee = "",
                                 labels = [| |], 
                                 milestone = 0)
//newIssue.JsonValue.Request "https://api.github.com/repos/fsharp/FSharp.Data/issues"

//------------------------------------------------
// Loading world bank data as JSON (using a schema)

type WorldBank = JsonProvider<"data/WorldBank.json">
let doc = WorldBank.GetSample()

let docAsync = WorldBank.AsyncLoad("http://api.worldbank.org/country/cz/indicator/GC.DOD.TOTL.GD.ZS?format=json")

docAsync |> Async.RunSynchronously


