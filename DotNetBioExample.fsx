
// Step 0. Boilerplate to get the paket.exe tool
 
#r "nuget:include=FSharp.Data, version=3.0.0"
#r "nuget:include=NetBio.Core, version=3.0.0-alpha"
 #r "netstandard"

open System
open System.Net
open System.IO
open Bio.IO.GenBank



let bacteriaTemplate = "ftp://ftp.ncbi.nlm.nih.gov/genomes/Bacteria/{0}/{1}.gbk"
let accession = "NC_002182"
let folder = "Chlamydia_muridarum_Nigg_uid57785";
let url = String.Format( bacteriaTemplate, folder, accession )

let downloader = new WebClient()

let  stream = downloader.OpenRead( url )
let reader = new StreamReader( stream )
let parser = new GenBankParser()
let sequence = parser.Parse(stream) |> Seq.head
let md = sequence.Metadata.["GenBank"] :?> GenBankMetadata
md.Accession
md.BaseCount
md.Comments
md.Contig
md.Definition

md.Features.Genes

//--------------------------------------------------------------------------

(*

// 1. TODO - create an FTP type provider

type NCBI = FtpProvider<"ftp://ftp.ncbi.nlm.nih.gov" (* , User="user", Password="password" *) >

let conn = NCBI.GetDataContext( (* "dynamic user", "dynamic password" *) )
//NCBI.genomes.Bacteria.Chlamydia_muridarum_Nigg_uid57785.``NC_002182.gbk``.Url   

let parser = new GenBankParser(NCBI.genomes.Bacteria.Chlamydia_muridarum_Nigg_uid57785.``NC_002182.gbk``.Url   )

*)




//NCBI.genomes.Bacteria.Chlamydia_muridarum_Nigg_uid57785.``NC_002182.gbk``.Replace()


(*
type GenBank = GenBankProvider<...>

let conn = GenBank.GetDataContext( "user", "password" )

let genome1 = conn.genomes.Bacteria.Chlamydia_muridarum_Nigg_uid57785.``NC_002182.gbk``
genome1.Accession
genome1.BaseCount
genome1.Comments
genome1.Contig

genome1.Definition
genome1.Features.sources.
genome1.Features.genes.TCA02.Range // "complement(1064..2086)"
genome1.Features.genes.TCA02.Range.IsComplement // "complement(1064..2086)"
genome1.Features.genes.TCA02.Range.Start // "1064
genome1.Features.genes.TCA02.Range.End // "2086"
genome1.Features.genes.TCA02.Id // "GeneID:1245521"
genome1.Features.genes.TCA02
genome1.Features.CDS
genome1.Features.misc

 



*)
