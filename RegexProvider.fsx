// Step 0. Boilerplate to get the paket.exe tool
 
open System


//-----------------------------------------------------------------------------------------
#r @"packages/FSharp.Text.RegexProvider/lib/net40/FSharp.Text.RegexProvider.dll"

open System.Text
open FSharp.Text.RegexProvider

open FSharp.Text.RegexProvider

type PhoneRegex = Regex< @"(?<AreaCode>^\d{3})-(?<PhoneNumber>\d{3}-\d{4}$)">

let regex = PhoneRegex()

regex.Ri

let results = PhoneRegex().Match("425-123-2345")


let areaCode = results.AreaCode.Value
