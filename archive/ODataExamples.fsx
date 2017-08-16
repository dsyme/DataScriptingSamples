#r "System.Data.Services.Client.dll"
#r "FSharp.Data.TypeProviders.dll"
#load "vizlib/show.fsx"
#load "extlib/ODataEx-0.1.fsx"

open Microsoft.FSharp.Data.TypeProviders

module NorthwindODataService = 

    open Microsoft.FSharp.Data.TypeProviders
    type Northwest = ODataService< "http://services.odata.org/Northwind/Northwind.svc/" >


    let db = Northwest.GetDataContext()
    db.DataContext.SendingRequest.Add (fun x -> printfn "requesting %A" x.Request.RequestUri)

    let allCustomersQuery = 
        query { for c in db.Customers do 
                select c }
        |> OData.executePaginated db.DataContext
        |> Seq.toList


    let allEmployeesQuery = 
        query { for c in db.Employees do 
                select c }
        |> teeGrid

    let allProductsQuery = 
        query { for c in db.Products do 
                select c }
        |> teeGrid

    let allOrdersQuery = 
        query { for c in db.Orders do 
                select c }
        |> teeGrid

    let firstFiveOrders = 
        query { for c in db.Orders do 
                take 5
                select c }
        |> teeGrid


    let ordersSortedByShipDateLatestFirst = 
        query { for o in db.Orders do
                sortByNullableDescending o.ShippedDate
                select (o.OrderID, o.ShippedDate) }
        |> teeGrid

    let ordersSortedByShipDateEarliestFirst = 
        query { for o in db.Orders do
                sortByNullable o.ShippedDate
                select (o.OrderID, o.ShippedDate) }
        |> teeGrid

    let ordersSortedByCustomerIDAndShipDateLatestFirst= 
        query { for o in db.Orders do
                sortBy o.CustomerID; thenByNullableDescending o.ShippedDate
                select (o.CustomerID, o.OrderID, o.ShippedDate) }
        |> teeGrid


    let ordersSortedByCustomerIDAndShipDateEarliestFirst= 
        query { for o in db.Orders do
                sortBy o.CustomerID; thenByNullable o.ShippedDate
                select (o.CustomerID, o.OrderID, o.ShippedDate) }
        |> teeGrid

    let allCustomersWithOrderCounts = 
        query { for c in db.Customers do 
                select c }
        |> Seq.map (fun c -> (c.ContactName,c.Orders.Count))
        |> teeGrid



    // This gives the orders properly, by returning the collection in the query
    let allCustomersWithOrderCounts = 
        query { for c in db.Customers do 
                select (c.City, c.Orders) }
        |> Seq.map (fun (c,o) -> (c,o.Count))
        |> teeGrid


    // This returns empty orders 
    let allCustomersQuery3_FAIL1 = 
        query { for c in db.Customers do 
                select c }
        |> Seq.map (fun c -> (c,c.Orders.Count))
        |> teeGrid

    // This returns empty orders 
    // http://services.odata.org/Northwind/Northwind.svc/Customers()?$expand=Orders
    let allCustomersQuery3_FAIL2 = 
        query { for c in db.Customers.Expand("Orders") do 
                select c }
        |> Seq.map (fun c -> (c.ContactName,c.Orders.Count))
        |> teeGrid
        |> ignore

    // This returns empty orders 
    let allCustomersQuery3_FAIL3 = 
        db.Customers.Expand("Orders")
        |> Seq.map (fun c -> (c.ContactName ,c.Orders.Count))
        |> teeGrid

    // Strangely, this does NOT return empty Order_Details!!!
    let allCustomersQuery4_FAIL2 = 
        query { for o in db.Orders.Expand("Order_Details") do 
                select o }
        |> Seq.map (fun o -> (o.CustomerID, o.Order_Details.Count))
        |> teeGrid
        |> ignore

    let customersWithOrderCounts_FAIL = 
        query { for c in db.Customers.Expand("Order_Details") do 
                //where (c.ContactName.Contains "Maria")
                select c }
        |> Seq.map (fun c -> (c.ContactName,c.Orders.Count))
        |> teeGrid

