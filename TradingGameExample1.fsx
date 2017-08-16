#load "packages/FSharp.Charting/FSharp.Charting.fsx"

open FSharp.Charting

type CardTypes = 
   | Hide 
   | Ochre 
   | Papyrus
   | Iron 
   | Salt
   | Timber 
   | Grain
   | Oil 
   | Wine
   | Cloth 
   | Bronze 
   | Silver
   | Spice 
   | Gem
   | Dye 
   | Gold
   | Ivory


let commodities = 
    [ (Hide, 1, 7)
      (Ochre, 1, 7)
      (Papyrus, 2, 5) 
      (Iron, 2, 7) 
      (Salt, 3, 9) 
      (Timber, 3, 8) 
      (Grain, 4, 8) 
      (Oil, 4, 7) 
      (Wine, 5, 4) // Should be 6, we're missing two cards! 
      (Cloth, 5, 7) 
      (Bronze, 6, 6)
      (Silver, 7, 5)
      (Spice, 7, 5)
      (Gem, 8, 5) 
      (Dye, 8, 4) 
      (Gold, 9, 5) 
      (Ivory, 9, 4) ]

Chart.Combine
 [ for (commodity, faceValue, maxCards)  in commodities -> 
     Chart.Line(Name=sprintf "%A" commodity, data=[ for x in 1 .. maxCards -> (x, faceValue*x*x) ])  ]

let maxPoints (commodity, faceValue, maxCards) = faceValue * maxCards * maxCards
let avgPoints ((commodity, faceValue, maxCards) as c) = maxPoints c / maxCards 
let avgPointsPerFacePoint ((commodity, faceValue, maxCards) as c) = maxPoints c / maxCards / faceValue


    
Chart.Column(Name="Count",data=[ for (commodity, faceValue, maxCards)  in commodities ->  sprintf "%A" commodity, maxCards ])
Chart.Column(Name="Count",data=[ for ((commodity, faceValue, maxCards) as c)  in commodities ->  sprintf "%A" commodity, maxPoints c ])

// TODO: how to label all points?
Chart.Column(data=[ for ((commodity, faceValue, maxCards) as c)  in commodities ->  sprintf "%A" commodity, avgPoints c ])


let square x = x * x

let maxValues = 
   [ for (commodity, faceValue, numCards)  in commodities ->  (commodity, square numCards * faceValue) ]

let maxiumPointCount = maxValues |> List.sumBy snd
   
maxiumPointCount / 7
maxiumPointCount / 3
