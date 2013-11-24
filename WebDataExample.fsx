
#load "extlib/EventEx-0.1.fsx"
#load "packages/FSharp.Charting/FSharp.Charting.fsx"
#load "vizlib--load-wpf.fsx"
#load "vizlib--show.fsx"
#load "examples--WebDataCredentials.fsx"







//-----------------------------------------------
// First some data scripting





let tableOfSquares = 
    [ for i in 0 .. 99 -> (i, i*i)  ] 


tableOfSquares |> showGrid

//-----------------------------------------------
// Now some charting

open FSharp.Charting


Chart.Line [ for i in 0 .. 99 -> (i, i*i) ]


let rnd = System.Random()
let rand() = rnd.NextDouble()

Chart.Point [ for i in 0 .. 10000 -> (rand(),rand()*rand()) ]
    
let form = new System.Windows.Forms.Form(Visible=true,TopMost=true)

form.MouseMove
   |> Event.map (fun e -> e.X, 500-e.Y) 
   |> LiveChart.LineIncremental

// FSharp.Charting 
//   UpdatingChart --> LiveChart


form.MouseMove
   |> Event.map (fun e -> System.DateTime.Now, 500-e.Y) 
   |> LiveChart.LineIncremental

form.MouseMove 
    |> Event.map (fun e -> e.Y) 
    |> Event.sampled 30 
    |> Event.windowTimeInterval 3000
    |> LiveChart.Line
 

//-----------------------------------------------
// Now some internet-scale data integration

// Reference the type provider, request the service types and get a data context



#r @"packages/FSharp.Data/lib/net40/FSharp.Data.dll"


open FSharp.Data
open FSharp.Data.FreebaseOperators

type Freebase = FreebaseDataProvider<Key=API_KEY,NumIndividuals=2000>
let data = Freebase.GetDataContext()


data.``Science and Technology``.Chemistry.``Chemical Elements``
  |> showGrid


// What's going on?
//
//  Gamma |- prog : type
//
// As far as the static analysis is concerned, the type of 
// 'data' ('FreebaseService') is not fully realized in Gamma. 
// Logically speaking it incorporates ALL of the schema of the
// entity graph (a graph of ~20,000 related types). 
//
// If the program includes operations related to the type
// then the provider will be interrogated for more information
// about the type.








data.``Science and Technology``.Chemistry.``Chemical Bonds`` |> showGrid
data.``Science and Technology``.Chemistry.``Chemical Elements`` |> showGrid









let aminoAcids = data.``Science and Technology``.Biology.``Amino Acids``


aminoAcids |> showGrid

let elements = data.``Science and Technology``.Chemistry.``Chemical Elements``


[ for e in elements do
    yield e.Name, e.``Boiling Point`` ]
  |> showGrid



data.``Arts and Entertainment``.Books.Books

data.``Science and Technology``.Computers.``Computer Scientists``.Individuals


//data.Sports.Soccer.``Football players``.Individuals.

// ------------------------------------------
// Query the 3 million books..
// Using remote-executing queies instead....


let books = data.``Arts and Entertainment``.Books.Books



query { for book in books do
        where (book.Name.ApproximatelyMatches("Atomic"))
        take 10 }

  |> showGrid








// ------------------------------------------
// Implement a feature: auto-suggest

let bookSuggest  (text : string) = 
    query { for e in books do 
            where (e.Name.ApproximatelyMatches(text))
            take 10 }

bookSuggest "Taco" |> show
bookSuggest "Boston" |> show

let comics = data.``Arts and Entertainment``.Comics

let characterSuggest  text = 
    query { for e in comics.``Comic Book Characters`` do 
            where (e.Name.Contains(text))
            take 20 }
      |> Seq.toList

characterSuggest "C" |> show
characterSuggest "O" |> show

let characterSuggestionsTable = 
   dict [ for c in 'A' .. 'Z' -> (c, characterSuggest (string c)) ]


let testForm = new System.Windows.Forms.Form(Visible=true, TopMost = true)

testForm.KeyPress.Add (fun x -> 
    show characterSuggestionsTable.[x.KeyChar])






//-------------------------------------------------------------------------------------
// Providing individuals.
//
// Consider a data Space: A1, A2, A3, B1, B2, B3
// 
// API #1 (completely heterogeneous)
//      data: obj list
// 
// API #2 (collections by type)
//      As: A list
//      Bs: B list
// 
// API #3 (named individuals)
//      A1: A 
//      A2: A 
//      A3: A 
//      B1: B 
//      B2: B 
//      B3: B 

elements |> Seq.find (fun x -> x.Name = "Hydrogen")


data.``Science and Technology``.Chemistry.``Chemical Elements``.Individuals




//data.``Science and Technology``.Medicine.``Infectious Diseases``.

data.``Science and Technology``.Chemistry.``Chemical Elements``.Individuals.Gold.``Melting Point`` |> showGrid


//elements.Individuals.Hydrogen.``Boiling Point``

//elements.Individuals.Gold.``Boiling Point``

//data.``Science and Technology``.Computers.``Computer Scientists``.Individuals.

data.``Science and Technology``.Medicine.``Infectious Diseases``.Individuals




// ------------------------------------------
// Explore the units of measure integration....

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let cyclones = data.``Science and Technology``.Meteorology.``Tropical Cyclones``

let topWind = cyclones.Individuals.``Hurricane Sandy``.``Highest winds``.Value



// ------------------------------------------
// That's all



















(*

chemistry.``Chemical Elements``.Individuals.Hydrogen.``Atomic radius``


computers.``Computer Scientists`` .Individuals.``Leslie Lamport``.``Country of nationality``



/// Count the stars listed in the database
let numberOfStars = astronomy.Stars.Count()


/// The name and distances of stars which have a distance recorded.
let someStarDistances = 
    query { for e in astronomy.Stars do 
            where e.Distance.HasValue
            select (e.Name, e.Distance) } 
      |> showGrid

//data.``Time and Space``.``Measurement Unit``.``Unit of Length``.Individuals.Parsec.``Distance In Meters``


/// Get the stars in the database sorted by proximity to earth
let starsSortedByProximityToEarth = 
    query { for e in astronomy.Stars do 
            sortBy e.Distance.Value
            take 10
            select e.Distance } 
      |> showGrid

/// Count the stars listed in the database
let getSomeCloseStars = 
    query { for e in astronomy.Stars do 
            where (e.Distance.Value < 4.011384e+18<_>)
            select e } 
      |> showGrid

/// Get the first 10 books matching a user string. 
let topBooksWithNameContaining (s:string) = 
    query { for book in data.``Arts and Entertainment``.Books.Books do
            where (book.Name.ApproximatelyMatches s)
            take 10 
            select book.Name }

topBooksWithNameContaining "1984" |> Seq.toList
topBooksWithNameContaining "Horse" |> Seq.toList
    







query {
    for x in data.Commons.Meteorology.``Tropical Cyclones`` do
    where (x.Damages <> null)
    } 
    |> Seq.length

query {
    for x in data.Commons.Meteorology.``Tropical Cyclones`` do
    where (x.Damages <> null)
    select (x.Name, x.Damages.Currency) }
  |> Seq.toList

query { for e in computers.``Computer Scientists`` do
        where (e.Name.ApproximatelyMatches "Jones")
        sortBy e.Name }
  |> Seq.length

query { for e in chemistry.``Chemical Elements`` do
        where e.``Atomic number``.HasValue
        sortBy e.``Atomic number``.Value }
  |> showGrid

query { for e in chemistry.``Chemical Elements`` do
        where e.``Atomic number``.HasValue
        sortBy e.Name
        select e.``Boiling Point`` }
  |> Seq.length

query { for e in computers.``Computer Scientists`` do
        where (e.Name.ApproximatelyMatches "Jones")
        sortBy e.Name }
  |> Seq.length

query { for e in computers.``Computer Scientists`` do
        where (e.Name.ApproximatelyMatches "Jones")
        count }

query { for e in books do
        where (e.Name.ApproximatelyMatches "Jones")
        count }


query { for e in books do
        where (e.Name.ApproximatelyMatches "1984")
        count }

query { for e in books do
        where (e.Name.ApproximatelyMatches "1985")
        count }
query { for e in books do
        where (e.Name.ApproximatelyMatches "1986")
        count }
*)
(*
open System.Linq.Expressions
let e = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.QuotationToExpression <@  null @>
let (|Constant|_|) (e:Expression) = 
    match e.NodeType, e with 
    | ExpressionType.Constant, (:? ConstantExpression as ce) ->  Some (ce.Value, ce.Type)
    | _ -> None
let (|Null|_|) (e:Expression) = 
    match e with
    | Constant(null,_) -> Some ()
    | _ -> None
match e with Null -> 1
*)

//[{ "/type/object/type" : "/meteorology/tropical_cyclone" , "type" : "/meteorology/tropical_cyclone" , "/type/object/id" : null , "/type/object/name" : null , "/meteorology/tropical_cyclone/category" : [], "/meteorology/tropical_cyclone/formed" : null, "/meteorology/tropical_cyclone/dissipated" : null, "/meteorology/tropical_cyclone/highest_winds" : null, "/meteorology/tropical_cyclone/lowest_pressure" : null, "/meteorology/tropical_cyclone/damages" : null, "/meteorology/tropical_cyclone/direct_fatalities" : null, "/meteorology/tropical_cyclone/indirect_fatalities" : null, "/meteorology/tropical_cyclone/total_fatalities" : null, "/meteorology/tropical_cyclone/affected_areas" : [], "/meteorology/tropical_cyclone/tropical_cyclone_season" : [], "/common/topic/alias" : [], "/common/topic/article" : [], "/common/topic/image" : [], "/common/topic/webpage" : [], "/common/topic/subjects" : [], "/common/topic/subject_of" : [], "/common/topic/properties" : [], "/common/topic/weblink" : [], "/common/topic/notable_for" : [], "/common/topic/notable_types" : [], "/common/topic/social_media_presence" : [], "/common/topic/official_website" : [], "/common/topic/topical_webpage" : [], "/common/topic/topic_equivalent_webpage" : [], "/common/topic/description" : [] , "limit": 500}]

// requesting 'http://freebaseread.com/api/service/mqlread?query={"cursor":true,"query":[{"/type/object/id":null, "/type/object/name":null , "/measurement_unit/dated_money_value/valid_date" : null, "/measurement_unit/dated_money_value/amount" : null, "/measurement_unit/dated_money_value/currency" : null, "/measurement_unit/dated_money_value/source" : null, "optional":true, "/type/object/type":"/measurement_unit/dated_money_value", "!/meteorology/tropical_cyclone/damages": [{"/type/object/id":"/en/tropical_storm_thelma","/type/object/type":"/meteorology/tropical_cyclone" , "limit": 500}]}]}'

(*


let cyclones = [ for x in data.Commons.Meteorology.``Tropical Cyclones`` -> x ]







// WebData demo












1


computers.``Computer Scientists`` |> showGrid

computers.``Programming Languages`` |> showGrid


let radi = 
   [ for x in chemistry.``Chemical Elements`` -> x.Name, x.``Atomic radius``  ]
   |> teeGrid



chemistry.``Chemical Elements``


//.Individuals.Hydrogen.``Atomic radius``



query { for b in books do 
        where (b.Name.ApproximatelyMatches "U") 
        select b }
  |> showGrid

//----------------------------------------------------------------------------------------------
// Query tests





module ErrorCases = 
    /// Unrecognized operation 'GroupBy' in web data query
    let _ = 
        query { for i in chemistry.Isotopes do 
                where (i.``Mass number`` ?<= 100)
                groupBy (i.``Mass number``) }

    /// Unrecognized operation 'Join' in web data query
    let _ = 
        query { for i in chemistry.Isotopes do 
                join e in chemistry.``Chemical Elements``  on (i.Name = e.Name) 
                select i.Name }


// TODO: specific overloads of Where etc. for collection types
// elements.Where(fun x -> x.Symbol = "H")
let elements = chemistry.``Chemical Elements``  
let isotopes = chemistry.Isotopes

query { for elem in (elements |> Seq.readonly) do 
        where (elem.Symbol = "U") 
        select elem }

query { for elem in elements do 
        where (elem.Symbol = "U") 
        select elem }

query { for elem in elements.AsEnumerable() do 
        where (elem.Symbol = "U") 
        select elem }

query { for elem in elements do 
        where (elem.Symbol.ApproximatelyMatches("C")) 
        select elem }

query { for elem in elements do 
        sortByNullable elem.``Atomic number``
        select elem }

query { for elem in elements do 
        sortByNullableDescending elem.``Atomic number``
        select elem }

query { for i in isotopes do 
        sortByNullable i.``Mass number``
        select i }

query { for i in isotopes do 
        sortByNullable i.``Mass number``
        thenBy i.Name }

query { for i in isotopes do 
        sortBy i.Name }

// FAILS
//query { for elem in elements do 
//        where (elem.Symbol.ApproximatelyOneOf()) 
//        select elem }

// FAILS - timeout
//query { for i in books do
//        sortBy i.Name }

query { for elem in elements do 
        where (elem.Symbol.ApproximatelyMatches("C*")) 
        select elem }

query { for elem in elements do 
        where (elem.Symbol.ApproximatelyOneOf("U")) 
        select elem }

query { for elem in elements do 
        where (elem.Symbol.ApproximatelyOneOf("U", "Na")) 
        select elem }

query { for elem in elements do 
        where (elem.Symbol = "U") 
        select elem.Name }

query { for elem in elements do 
        where (elem.Symbol = "U") 
        select (elem.Name, elem.``Atomic mass``) }

query { for elem in elements do 
        select elem.``Atomic mass`` } 

query { for elem in elements do 
        select (elem.Name, elem.Discoverer) } 

query { for elem in elements do 
        let discoverers = query { for x in elem.Discoverer -> x }
        select (elem.Name, discoverers) } 

query { for elem in elements do 
        where (elem.Discoverer.Count() > 0)
        select (elem.Name, elem.Discoverer) } 

query { for elem in elements do 
        where (elem.Symbol = "Na") 
        select elem }

query { for elem in elements do 
        where (elem.Symbol = "H") 
        select elem }

query { for elem in elements do 
        where (elem.Name = "Hydrogen") 
        select elem }

query { for elem in elements do 
        where (elem.Name.ApproximatelyMatches "*anium") 
        select elem }

query { for book in books do 
        where (book.``Date written`` = "1998")
        select book }

query { for elem in elements do 
        where (elem.``Atomic number`` ?= 6)
        select elem }

query { for elem in elements do 
        where (elem.``Atomic number`` ?<> 6)
        select elem }

query { for elem in elements do 
        where (elem.``Atomic number`` ?< 6)
        select elem }

// BUG: should return zero elements
query { for elem in elements do 
        where (elem.``Atomic number`` ?= 6)
        where (elem.``Atomic number`` ?= 7)
        select elem }
        |> Seq.length

query { for i in isotopes do 
        where (i.``Mass number`` ?> 100)
        select i }
        |> Seq.length

query { for i in isotopes do 
        where (i.``Mass number`` ?>= 100)
        select i }
        |> Seq.length

query { for i in isotopes do 
        where (i.``Mass number`` ?< 100)
        select i }
        |> Seq.length

query { for i in isotopes do 
        where (i.``Mass number`` ?<= 100)
        select i }
        |> Seq.length

query { for i in isotopes do 
        where (i.``Mass number`` ?> 150)
        select i }
        |> Seq.length

query { for i in isotopes do 
        where (i.``Mass number`` ?> 200)
        select i }
        |> Seq.length

query { for i in isotopes do 
        where (i.``Mass number`` ?> 220)
        select i }
        |> Seq.length


// get formulae
query { for elem in biology.``Amino Acids`` do 
        select elem.Formula }

query { for elem in biology.``Amino Acids`` do 
        where (elem.Formula = "C5H9NO2") 
        select elem }


*)

(*
//computers.``Computer Scientists``.Individuals

//computers.``Programming Language Designers``.Individuals

//chemistry.Isotopes
chemistry.``Chemical Elements``.Individuals.Hydrogen.``Atomic mass``.Mass

chemistry.``Chemical Elements`` |> showGrid

chemistry.``Chemical Elements``.Individuals.Hydrogen |> showGrid

data.Books.Books.Individuals.``Brave New World``.Editions

data.Baseball.``Baseball Teams``.Individuals.``New York Mets``.``Current Coaches``  |> show

data.Soccer.``Football player`` |> showGrid


// A more complex "answer me a question" query: "What are the asteroids with the name of a greek god?"

// Step 1. find the gods
data.Religion.Deity  |> showGrid

// Step 2. find the greek god names
let godNames = 
    query { for d in data.Religion.Deity do
            for r in d.``Religion(s) That Worship This Deity`` do
            where (r.Name.Contains "Greek")
            select d.Name } 
      |> Seq.toList
      |> teeGrid


// Step 3. find the asteroid names

data.Astronomy.Asteroid  |> showGrid

let words (s:string) = s.Split(' ') 

words "hello world" |> show

query { for a in data.Astronomy.Asteroid  do 
        select (a.Name, words a.Name) }
  |> teeGrid

// Step 4. find the asteroids whose name is a greek god

query { for a in data.Astronomy.Asteroid  do
        where (godNames.Intersect(words a.Name).Count() > 0) }


computers.``Computer Scientist`` |> showGrid

computers.``Programming Language`` |> showGrid


let radi = 
   [ for x in chemistry.``Chemical Element`` -> x.Name, x.``Atomic radius``  ]
   |> teeGrid

let vdwr = 
    [ for x in chemistry.``Chemical Element`` -> x.Name, x.``Van der Waals radius``  ]
    |> teeGrid

let ratios = 
    query { for x in chemistry.``Chemical Element`` do 
            where x.``Atomic radius``.HasValue 
            where x.``Van der Waals radius``.HasValue 
            select (x.Name, x.``Atomic radius``.Value / x.``Van der Waals radius``.Value)  }
    |> teeGrid

let averageVanDerWaalsRadius = 
    query { for x in chemistry.``Chemical Element`` do 
            where x.``Van der Waals radius``.HasValue 
            averageBy  x.``Van der Waals radius``.Value  }


















    *)

// Kilopascal
// Megapascal
// Degree Celsius
// Joule per mole per kelvin
// Kilojoule per mole
// Joule per mole per kelvin
// Gram per cubic centimeter

(*    
data.``topic-en``.DomainIDs
[ for e in data.``Measurement Unit``.``Unit Of Length`` do if e.``Distance In Meters``.HasValue then  yield e.Id, e.Name, e.``Distance In Meters``.Value ]

let si = data.``Measurement Unit``.``Measurement System`` |> Seq.find (fun x -> x.Name = "International System of Units")

sprintf "%.12g" 1.0e-12
sprintf "%.12f" 1.00000001

type MU = data.DataTypes.Measurement_unit

let t1 = for x in si.``Length Units`` do fmtLengthUnit x
let t2 = [ for x in si.``Mass Units`` -> x.Id, x.Name, x.``Weightmass In Kilograms`` ]


data.``Measurement Unit``.``Unit of Force`` |> showGrid
*)


//data.UnitSystems.SI.UnitNames.gray




(*


[ for x in 0 .. 99 -> (x, x * x) ] |> showGrid




open Microsoft.FSharp.Linq

query { for e in chemistry.``Chemical Element``  do
        where e.``Atomic number``.HasValue
        sortBy (e.``Atomic number``.GetValueOrDefault()) }
  |> showGrid



// // Real-world, really interlinked
//chemistry.``Chemical Element``.Head.Discoverer.Head.Discovered.Head.``Discovering Country``.Head.``Administrative Divisions``.Head.

//chemistry.``Chemical Element``.Head.Discoverer.Head.Discovered.Head.``Discovering Country``.Head.





//data.Astronomy.Asteroid

//data.Religion.Deity
let godNames = 
    seq { for d in data.Religion.Deity do
             for r in d.``Religion(s) That Worship This Deity`` do
                 if r.Name.Contains "Greek" then 
                     yield d.Name } 
    |> Seq.distinct
    //|> teeGrid


let gods = set godNames

gods.Count 

let words (s:string) = s.Split(' ') 

words "hello world"

data.Astronomy.Asteroid  |> showGrid

seq { for a in data.Astronomy.Asteroid do 
          yield [words a.Name] }
  |> showGrid

seq { for a in data.Astronomy.Asteroid do 
          yield a.Name |> words, a.``Member of Asteroid Family`` } |> showGrid



seq {  for asteroid in data.Astronomy.Asteroid do
          if words asteroid.Name |> Seq.exists (fun w -> gods.Contains w) then 
              yield asteroid }
     |> showGrid









chemistry.``Chemical Element`` |> showGrid


[ for e in chemistry.``Chemical Element`` do 
    yield e.``Atomic number``, e.Name, e.``Boiling Point`` ] 
       |> showGrid


[ for e in chemistry.``Chemical Element`` do 
    if e.``Atomic number``.HasValue then 
       yield e.``Atomic number``, e.Name, e.``Boiling Point`` ] 
       |> showGrid








data.Astronomy.Asteroid |> showGrid

chemistry.``Chemical Element`` |> showGrid





//-----------------------------------------------------------------------------------

[ for x in 1 .. 100 -> (x, x * x) ] |> Chart.Line
[ for x in 1 .. 100 -> (x, x * x) ] |> Chart.Bar
[ for x in 1 .. 100 -> (x, x * x) ] |> Chart.Column
[ for x in 1 .. 100 -> (x, x * x) ] |> Chart.Point
[ for x in 1 .. 10 -> (string x, x * x) ] |> Chart.Pie
[ for x in 1 .. 100 -> (x, x * x) ] |> showGrid










biology.``Amino Acid`` |> showGrid

seq {  for aa in biology.``Amino Acid`` do 
          for dc in aa.``DNA Codons`` do 
             if dc.Contains "T" then
                yield (aa.Name, dc) }
 |> showGrid

biology.Animal.Head |> showGrid





biology.``Breed group`` |> showGrid


chemistry.``Chemical Element`` |> showGrid


chemistry.``Chemical Element`` 
 |> Seq.where (fun e -> e.``Atomic number``.HasValue)
 |> Seq.sortBy (fun e -> e.``Atomic number``.GetValueOrDefault() )
 |> showGrid

query { for e in chemistry.``Chemical Element`` do
        for d in e.Discoverer do
        groupBy d.Name into g
        sortBy g.Length 
        take 1 }


chemistry.``Chemical Element`` |> showGrid

[ for x in 0 .. 99 -> (x, x*x) ] |> Chart.Line

//data.Baseball.``Baseball Player``.Head. |> showGrid


data.Business.``Oil Field`` |> showGrid

data.Business.``Business Operation`` |> showGrid

computers.``Computer Scientist`` |> showGrid

data.Olympics.``Olympic athlete`` |> showGrid

data.Film.Film |> showGrid

query { for film in data.Film.Film do 
        where (film.Genres.Contains "Thriller")
        select film } 
    |> showGrid


[ for i in 0 .. 99 -> (i, i*i) ] |> showGrid

[ for i in 0 .. 99 -> i ] |> showGrid


data.Astronomy.Asteroid |> showGrid


//data.Religion.Deity.Head. |> showGrid
//data.Baseball.``Baseball Player``.Head.``Lifetime Batting Statistics`` |> showGrid

biology.``Amino Acid`` |> showGrid

chemistry.``Chemical Element`` |> showGrid


// Highly linked!!!
// This is the reality of data & relations in the real world.
//data.Automotive.``Automobile Model``.Head.``Manufactured At``.Head.``Manufacturing Plant``.Location.Head.``People born here``.Head.Children.Head.

data.Education.Dissertation |> showGrid

query {  for a in data.Astronomy.Asteroid do 
         select (a.Name, a.``Spectral Type``)  }
   |> showGrid

query { for c in data.``American football``.``American football head coach``
        select (c.Name,  c.``Coaching history``) }
   |> showGrid

let objs = chemistry.``Chemical Element`` 

data.Astronomy.Asteroid |> showGrid

seq { for a in data.Astronomy.Asteroid do 
         yield a.Name, a.``Spectral Type`` } |> showGrid

chemistry.``Chemical Element`` |> showGrid

data.Astronomy.Asteroid

data.Astronomy.Astronomer |> showGrid

biology.``Amino Acid`` |> showGrid

data.Architecture.Architect |> showGrid

data.Physics.``Subatomic particle`` |> showGrid

data.``American football``.``American football player`` |> showGrid

query { for l in computers.``Programming Language`` do
        sortBy l.Name }

query { for l in biology.``Amino Acid`` do
        sortBy l.Name }
    |> showGrid

data.Film.Film |> showGrid

// 1. view data
computers.``Programming Language`` 
  |> Seq.where (fun p -> p.``Language Paradigms`` |> Seq.exists (fun p -> p.Name.Contains("Assemb")))
  |> showGrid

computers.``Programming Language Paradigm`` |> showGrid

computers.``Programming Language`` 
  |> showGrid

// 2. clean data
computers.``Programming Language`` 
  |> Seq.where (fun p -> p.Introduced <> null)
  |> showGrid

// 3. clean data again
let succeeds f x = try f x |> ignore; true with _ -> false
computers.``Programming Language`` 
  |> Seq.where (fun p -> p.Introduced <> null)
  |> Seq.where (fun p -> not (succeeds int p.Introduced)) |> showGrid
  |> showGrid

// 4. display data 
computers.``Programming Language`` 
  |> Seq.where (fun p -> p.Introduced <> null)
  |> Seq.where (fun p -> succeeds int p.Introduced) 
  |> Seq.where (fun p -> p.``Language Paradigms`` |> Seq.exists (fun p -> p.Name.Contains("Functional")))
  |> Seq.countBy (fun p -> int p.Introduced / 5)
  |> Seq.map (fun (b,n) -> b * 5 + 1, n)
  |> Seq.sortBy (fun (b,n) -> b)
  |> Chart.Column

[ for f in data.Film.Film do
     yield f.``Gross revenue`` , f.``Estimated budget`` ]


chemistry.``Chemical Element``
    |> Seq.sortBy(fun  e -> e.``Atomic number``.GetValueOrDefault() )
    |> Seq.map(fun e -> e.Name, e.Isotopes |> Seq.sumBy (fun i -> 
                                        decimal i.``Mass number``.GetValueOrDefault() * 
                                        i.``Natural abundance (Earth)``.GetValueOrDefault()  ))
    |> Chart.Line

chemistry.``Chemical Element``
    |> Seq.sortBy(fun  e -> e.``Atomic number``.GetValueOrDefault() )
    |> Seq.map(fun e -> e.Name, e.``Atomic mass``)
    |> Seq.where(fun (a,b) -> match b with null -> true | _ -> false)
    |> Seq.length


computers.``Programming Language`` |> Seq.where (fun l -> l.Name.StartsWith "C") |> showGrid
computers.``Programming Language`` |> Seq.where (fun l -> l.Name.StartsWith "F") |> showGrid

data.Astronomy.Asteroid |> showGrid

seq {  for a in data.Astronomy.Asteroid  do 
         yield a.Name, a.``Spectral Type`` } |> show


chemistry.``Chemical Element`` |> showGrid

[ for e in chemistry.``Chemical Element`` do 
     yield e.Name, e.``Atomic number``, e.``Boiling Point`` ] |> show

seq {  for a in data.Astronomy.Asteroid do
          yield a.Name, a.``Spectral Type`` }
  |> show   

data.Architecture.Architect |> show

data.Astronomy.Asteroid |> show

chemistry.``Chemical Element`` |> show

data.Olympics.``Olympic athlete`` |> showGrid

data.Spaceflight.``Space Mission`` |> showGrid

biology.``Amino Acid`` |> showGrid

let getPeriodicTable() = 
    [ for e in chemistry.``Chemical Element`` do
         yield e.Name, e.``Atomic number``, e.``Boiling Point`` ] 
     |> Seq.sortBy (fun (nm,_,_) -> nm)
     |> Seq.toList

getPeriodicTable() |> show


let getElementData() = 
    [ for e in chemistry.``Chemical Element`` do 
          yield e.``Atomic mass`` ]

[ for e in chemistry.``Chemical Element`` do 
     yield e.Name, e.``Boiling Point`` ] 
  
  |> showGrid


[ for elem in elements do 
     yield elem.Name ]
     |> Seq.sort
     |> show

elements 
     |> Seq.where (fun e -> e.``Atomic number``.HasValue)
     |> Seq.sortBy (fun e -> e.``Atomic number``.GetValueOrDefault())
     |> showGrid
     
let superBowls = data.``American football``.``Super bowl``

superBowls |> showGrid

let p1 = data.Baseball.``Baseball Player`` |> Seq.head

p1 |> show

//-----------------------------------------
// Queries
//
//  Tasks:
//       - find hydrogen
//       - find all elements whose boiling point is > 5000C
//  

[ for e in chemistry.``Chemical Element`` do
    yield e.Name, e.``Atomic number`` ]
  |> Seq.sortBy (fun (n,_) -> n)
  |> show

let hydrogen = 
   chemistry.``Chemical Element``
        |> Seq.find (fun e -> e.Name.Contains "Hydrogen")
   
[ hydrogen ] |> showGrid

// A query 
[ for element in chemistry.``Chemical Element`` do
    if element.``Boiling Point`` .> 5000.0M then 
       yield element ]
 |> showGrid


//let's look at the data for football teams
let teams = data.``American football``.``American football team``

teams |> show

data.Celebrities.``Rehab facility``  |> show
data.Celebrities.Celebrity  |> show
data.Celebrities.``Criminal offense`` |> show
data.Celebrities.``Legal entanglement`` |> show
data.Celebrities.``Sexual orientation`` |> show
data.Celebrities.``Substance abuse problem`` |> show
[ for c in data.Celebrities.``Substance abuse problem`` do
     match c.Substance with 
     | null -> ()
     | _ -> 
         if c.Substance.Name = "Cocaine" then 
            yield c.Celebrity ]
         |> showGrid

//See what years the game schedule data covers
[ for t in teams do 
      for g in t.``Away Games`` do
         yield System.DateTime.Parse(g.Date).Year ] 
  |> Seq.distinct
  |> show

//let's define the standard deviation function to use on some distributions
let stdDevBy (f:'a -> float) seq =
    let seq2 = Seq.map f seq 
    let avg = Seq.average seq2
    seq2 |> Seq.map (fun x -> (x - avg) ** 2.) |> Seq.average |> sqrt



//--------------------------------------





let estimateLikelihoodOfSubstanceAbuse
       (celebrity: data.DataTypes.Celebrities.CelebrityData) =
    if celebrity.Name.Contains "..." then 95.0 else 15.0


// Task: write a function that estimates the likelihood of substance abuse by a celberity by
// name. Note we are using the online strongly typed model of celebrities as entities.
let estimateLikelihoodOfSubstanceAbuse
       (celebrity: data.DataTypes.Celebrities.CelebrityData) =
    if celebrity.Name.Contains "..." then 95.0 else 15.0


[ for celeb in data.Celebrities.Celebrity do
    yield (celeb.Name, estimateLikelihoodOfSubstanceAbuse celeb) ]


*)





(*
query { for e in data.``Measurement Unit``.``Unit Of Length`` do 
        where e.``Distance In Meters``.HasValue 
        select (e.Name, e.``Distance In Meters``.Value) }
  |> showGrid

chemistry.``Chemical Elements`` |> showGrid

let hydrogen = 
    query { for e in chemistry.``Chemical Elements`` do 
            where (e.Name = "Hydrogen")
            head }


let helium = 
    query { for e in chemistry.``Chemical Elements`` do 
            where (e.Name = "Helium")
            head }

let radius1 = hydrogen.``Atomic radius``.GetValueOrDefault()
let radius2 = helium.``Atomic radius``.GetValueOrDefault()

let twoHR = radius1 * 2.0


[<Measure>] type picometre

let metrePerPicometre = 1.0e-12<metre/picometre>


let twoHR = radius1 + 2.0<metre>

let x = radius1 + 2.0<picometre>

let y = radius1 + (2.0<picometre> * metrePerPicometre)

*)


(*
#if COMPILED
[<STAThread>]
do()
#endif


type Unit = 
    | SI of string 
    | Prod of Unit * Unit 
    | Div of Unit * Unit
    | One
    static member (*) (u1: Unit, u2:Unit) = Prod (u1, u2)
    static member (/) (u1: Unit, u2:Unit) = Div (u1, u2)
    override u.ToString() = 
        match u with
        | SI s -> "SI \"" + s + "\""
        | Prod (u1,u2) -> "Prod (" + u1.ToString() + "," + u2.ToString() + ")"
        | Div (u1,u2) -> "Div (" + u1.ToString() + "," + u2.ToString() + ")"
        | One -> "One"

//1.7e-27 |> sprintf "%g"

//data.``Measurement Unit``.``Unit of Mass``.Individuals.``Atomic mass unit``.``Weightmass In Kilograms``
let mus = data.``Measurement Unit``
let fmt (units: seq<#Samples.DataStore.Freebase.FreebaseData.DataTypes.Freebase.Freebase.Unit_profileData>) 
        (txt:Unit) 
        (getMultiplier: _ -> Nullable<float<'u>>) 
        (getOffset: (_ -> float<'u>) option) = 
    for e in units do 
       let multiplier : Nullable<_> = getMultiplier e 
       if multiplier.HasValue then  
           let multiplierText = sprintf "%g" (double multiplier.Value)
           let multiplierText = if multiplierText |> String.forall System.Char.IsDigit then multiplierText+".0" else multiplierText
           let offset = 
               match getOffset with None -> None | Some f -> Some (f e)
           let offset = 
               match offset with 
               | Some v  -> 
                   let s = sprintf "%g" (double v)
                   if s |> String.forall System.Char.IsDigit then "Some "+s+".0" else "Some "+s
               | _ -> "None"
           let cleanAbbrev x =
                    [
                                ("\"","\\\"")
                                ("²","^2")
                                ("³","^3")
                                ("µ","u")
                    ] |> Seq.fold (fun (x:string) (a,b) -> x.Replace(a,b)) x
           let abbrevs = 
                e.``Abbreviation(s)`` 
                |> Seq.map (cleanAbbrev)
                |> Seq.map (fun s -> "\"" + s + "\"") |> String.concat "; " 
           let canonicalAbbrev = e.``Canonical Abbreviation`` |> (fun s -> "\"" + s + "\"")  |> cleanAbbrev
           printf """       ("%s", (* "%s", *) [ %s ], %s, (%s, %s, %s))""" e.Id e.Name abbrevs canonicalAbbrev (string txt) multiplierText offset
                   
           

let m = SI "metre"
let s = SI "second"
let kg = SI "kilogram"
let kelvin = SI "kelvin"
let joule = SI "joule"
let siemens = SI "siemens"
let newton = SI "newton"
let pascal = SI "pascal"
let watt = SI "watt"
let radian = One
let steradian = One
let square (u:Unit) = u * u 
let cubic (u:Unit) = u * u * u
printfn "let units = "
printfn "   dict ["
fmt mus.``Unit of Length`` (m) (fun x -> x.``Distance In Meters``) None
fmt mus.``Unit of Area`` (square m) (fun x -> x.``Area In Square Meters``) None
fmt mus.``Unit of Energy`` (joule) (fun x -> x.``Energy in Joules``) None
fmt mus.``Unit of Pressure`` (pascal) (fun x -> x.``Pressure in pascals``) None
fmt mus.``Unit of Power`` (watt) (fun x -> x.``Power In Watts``) None
fmt mus.``Unit of Speed`` (m / square s) (fun x -> x.``Speed in meters per second``) None
fmt mus.``Unit Of Volume`` (cubic m) (fun x -> x.``Volume In Cubic Meters``) None
fmt mus.``Unit of Density`` (kg / cubic m) (fun x -> x.``Density in kilograms per cubic meter``) None
fmt mus.``Unit of Time`` (s) (fun x -> x.``Time In Seconds``) None
fmt mus.``Unit of Mass`` (kg) (fun x -> x.``Weightmass In Kilograms``) None
fmt mus.``Unit of Absorbed Dose`` (SI "gray") (fun x -> x.``Dose in grays``) None
fmt mus.``Unit of Absorbed Dose Rate`` (SI "gray" / s) (fun x -> x.``Rate in grays per second``) None
fmt mus.``Unit of Acceleration`` (m / square s) (fun x -> x.``Acceleration in meters per second squared``) None
fmt mus.``Unit of Amount Concentration`` (SI "mole" / cubic m) (fun x -> x.``Concentration in moles per cubic meter``) None
fmt mus.``Unit of Amount of Substance`` (SI "mole") (fun x -> x.``Amount in moles``) None
fmt mus.``Unit of Angular Acceleration`` (radian / square s) (fun x -> x.``Acceleration in radians per second squared``) None
fmt mus.``Unit of Angular Frequency`` (radian / s) (fun x -> x.``Frequency in radians per second``) None
fmt mus.``Unit of Capacitance`` (SI "farad") (fun x -> x.``Capacitance in farads``) None
fmt mus.``Unit of Catalytic Activity`` (SI "katal") (fun x -> x.``Activity in katals``) None
fmt mus.``Unit of Catalytic Concentration`` (SI "katal" / cubic m) (fun x -> x.``Concentration in katals per cubic meter``) None
fmt mus.``Unit of Charge`` (SI "coulomb") (fun x -> x.``Charge in coulombs``) None
fmt mus.``Unit of Conductance`` (siemens) (fun x -> x.``Conductance in siemens``) None
fmt mus.``Unit of Conductivity`` (siemens / m) (fun x -> x.``Conductivity in siemens per meter``) None
fmt mus.``Unit of Current Density`` (SI "ampere" / square m) (fun x -> x.``Density in amperes per square meter``) None
//fmt mus.``Unit of data Size`` (siemens) (fun x -> x) None
//fmt mus.``Unit of data Transmission Rate`` (siemens) (fun x -> x) None
fmt mus.``Unit of Dose Equivalence`` (SI "sievert") (fun x -> x.``Equivalence in sieverts``) None
fmt mus.``Unit of Electric Charge Density`` (SI "coulomb" / cubic m) (fun x -> x.``Density in coulombs per cubic meter``) None
fmt mus.``Unit of Electric Current`` (SI "ampere") (fun x -> x.``Current in amperes``) None
fmt mus.``Unit of Electric Field Strength`` (SI "volt" / m) (fun x -> x.``Strength in volts per meter``) None
fmt mus.``Unit of Electric Flux Density`` (SI "coulomb" / square m) (fun x -> x.``Density in coulombs per square meter``) None
fmt mus.``Unit of Energy Density`` (joule / cubic m) (fun x -> x.``Density in joules per cubic meter``) None
fmt mus.``Unit of Exposure`` (SI "coulomb" / kg) (fun x -> x.``Exposure in coulombs per kilogram``) None
fmt mus.``Unit of Force`` (SI "newton") (fun x -> x.``Force in newtons``) None
//fmt mus.``Unit of Fuel Economy`` (siemens) (fun x -> x) None
fmt mus.``Unit of Heat Capacity`` (joule / kelvin) (fun x -> x.``Capacity in joules per kelvin``) None
fmt mus.``Unit of Illuminance`` (SI "lux") (fun x -> x.``Illuminance in lux``) None
fmt mus.``Unit of Inductance`` (SI "henry") (fun x -> x.``Inductance in henries``) None
fmt mus.``Unit of Inverse Temperature`` (One / kelvin) (fun x -> x.``Temperature coefficient in inverse kelvins``) None
fmt mus.``Unit of Irradiance`` (watt / square m) (fun x -> x.``Irradiance in watts per square meter``) None
fmt mus.``Unit of Luminance`` (SI "candela" / square m) (fun x -> x.``Luminance in candelas per square meter``) None
fmt mus.``Unit of Luminous Flux`` (SI "lumen") (fun x -> x.``Flux in lumens``) None
fmt mus.``Unit of Luminous Intensity`` (SI "candela") (fun x -> x.``Intensity in candelas``) None
fmt mus.``Unit of Magnetic Field Strength`` (SI "ampere" / square m) (fun x -> x.``Strength in amperes per meter``) None
fmt mus.``Unit of Magnetic Flux`` (SI "weber") (fun x -> x.``Flux in webers``) None
fmt mus.``Unit of Magnetic Flux Density`` (SI "tesla") (fun x -> x.``Density in teslas``) None
fmt mus.``Unit of Molar Energy`` (joule / SI "mole") (fun x -> x.``Energy in joules per mole``) None
fmt mus.``Unit of Molar Heat Capacity`` ((joule / SI "mole") / kelvin) (fun x -> x.``Capacity in joules per mole per kelvin``) None
//fmt mus.``Unit of Molar Mass`` (SI "gram" / SI "mole") (fun x -> x.``Molar mass in grams per mole``) None
fmt mus.``Unit of Molar Volume`` (cubic m / SI "mole") (fun x -> x.``Volume in cubic meters per mole``) None
fmt mus.``Unit of Moment of Force`` (SI "newton" * m) (fun x -> x.``Moment in newton meters``) None
fmt mus.``Unit of Permeability`` (SI "henry" / m) (fun x -> x.``Permeability in henries per meter``) None
fmt mus.``Unit of Permittivity`` (SI "farad" / m) (fun x -> x.``Permittivity in farads per meter``) None
//fmt mus.``Unit of Plane Angle`` (siemens) (fun x -> x) None
fmt mus.``Unit of Potential`` (SI "volt") (fun x -> x.``Potential in volts``) None
fmt mus.``Unit of Radiance`` (watt / square m / steradian) (fun x -> x.``Radiance in watts per square meter per steradian``) None
fmt mus.``Unit of Radiant Intensity`` (watt / steradian) (fun x -> x.``Intensity in watts per steradian``) None
fmt mus.``Unit of Radioactivity`` (SI "becquerel") (fun x -> x.``Radioactivity in becquerels``) None
fmt mus.``Unit of Resistance`` (SI "ohm") (fun x -> x.``Resistance in ohms``) None
fmt mus.``Unit of Resistivity`` (SI "ohm" * m) (fun x -> x.``Resistivity in ohm meters``) None
//fmt mus.``Unit of Solid Angle`` (siemens) (fun x -> x) None
fmt mus.``Unit of Specific Energy`` (joule / kg) (fun x -> x.``Energy in joules per kilogram``) None
//fmt mus.``Unit of Specific Fuel Consumption`` (siemens) (fun x -> x) None
fmt mus.``Unit of Specific Heat Capacity`` (joule / kg / kelvin) (fun x -> x.``Capacity in joules per kilogram per kelvin``) None
fmt mus.``Unit of Specific Volume`` (cubic m / kg) (fun x -> x .``Specific volume in cubic meter per kilograms``) None
fmt mus.``Unit of Surface Density`` (kg / square m) (fun x -> x.``Density in kilograms per square meter``) None
fmt mus.``Unit of Surface Tension`` (newton / m) (fun x -> x.``Tension in newtons per meter``) None
//fmt mus.``Unit of Symbol Rate`` (siemens) (fun x -> x) None
fmt mus.``Unit of Temperature`` (kelvin) (fun x -> x.``Kelvins Multiplier``)  (Some (fun x -> float x.``Zero point in kelvins``.Value)  )
fmt mus.``Unit of Thermal Conductivity`` (watt / m / kelvin) (fun x -> x.``Conductivity in watts per meter per kelvin``) None
fmt mus.``Unit of Viscosity`` (pascal * s) (fun x -> x.``Viscosity in pascal seconds``) None
fmt mus.``Unit of Volumetric Flow Rate`` (cubic m / s) (fun x -> x.``Rate in cubic metres per second``) None
fmt mus.``Unit of Wavenumber`` (One / m) (fun x -> x.``Wavenumber in reciprocal meters``) None
fmt mus.``Unit of frequency`` (SI "hertz") (fun x -> x.``Frequency in hertz``) None
printfn "  ]"
//..\Users\dsyme\libs\Chart-0.2\Chart.dll
//#load @"..\Users\dsyme\libs\Chart-0.2\ChartAutoDisplay.fsx"
*)
