module FileSystem =

    type FileSystemItem =
        | File of File
        | Directory of Directory
    and File = {name:string; fileSize:int}
    and Directory = {name:string; dirSize:int; subitems:FileSystemItem list}

    let rec cataFS fFile fDir item : 'r =
        let recurse = cataFS fFile fDir
        match item with
        | File file -> fFile file
        | Directory dir ->
            let listOfRs = dir.subitems |> List.map recurse
            fDir (dir.name,dir.dirSize, listOfRs)

    let totalSize fileSystemItem =
        let fFile (file:File) = file.fileSize
        let fDir (name,size,subsizes) = (List.sum subsizes) + size
        cataFS fFile fDir fileSystemItem

    let largestFile =
        let ifNone deflt opt =
            defaultArg opt deflt

        let fileSize fileOpt =
            fileOpt
            |> Option.map (fun file -> file.fileSize)
            |> ifNone 0

        let fFile (file:File) = Some file

        let fDir (name,size,subfiles) =
            match subfiles with
            | [] -> None
            | subfiles -> subfiles |> List.maxBy fileSize

        cataFS fFile fDir

let readme = FileSystem.File {name="readme.txt"; fileSize=1}
let config = FileSystem.File {name="config.xml"; fileSize=2}
let build  = FileSystem.File {name="build.bat"; fileSize=3}
let src = FileSystem.Directory {name="src"; dirSize=10; subitems=[readme; config; build]}
let bin = FileSystem.Directory {name="bin"; dirSize=10; subitems=[]}
let root = FileSystem.Directory {name="root"; dirSize=5; subitems=[src; bin]}

readme |> FileSystem.totalSize
src |> FileSystem.totalSize
root |> FileSystem.totalSize

readme |> FileSystem.largestFile
src |> FileSystem.largestFile
bin |> FileSystem.largestFile
root |> FileSystem.largestFile

module Products =
    type Product =
        | Bought of BoughtProduct
        | Made of MadeProduct
    and BoughtProduct = {
        name: string
        weight: int
        vendor: string option }
    and MadeProduct = {
        name: string
        weight: int
        components: Component list }
    and Component = {
        qty: int
        product: Product }

    let rec cataProduct fBought fMade product : 'r =
        let recurse = cataProduct fBought fMade

        let convertComponentToTuple comp =
            (comp.qty, recurse comp.product)

        match product with
        | Bought bought -> fBought bought
        | Made made ->
            let componentTuples =
                made.components
                |> List.map convertComponentToTuple
            fMade (made.name,made.weight,componentTuples)

    let productWeight =
        let fBought (bought:BoughtProduct) = bought.weight
        let fMade (name,weight,componentTuples) =
            let componentWeight (qty,weight) = qty * weight
            let totalComponentWeight =
                componentTuples |> List.sumBy componentWeight
            totalComponentWeight + weight
        cataProduct fBought fMade

    type VendorScore = {vendor:string; score:int}

    let mostUsedVendor =
        let fBought (bought:BoughtProduct) =
            bought.vendor |> Option.map (fun vendor -> {vendor = vendor; score = 1})
        let fMade (name,weight,subresults) =
            let vendor vs = vs.vendor
            let score vs = vs.score

            let totalScore (vendor,vendorScores) =
                let totalScore = vendorScores |> List.sumBy score
                {vendor=vendor; score=totalScore}

            subresults
            |> List.choose snd
            |> List.groupBy vendor
            |> List.map totalScore
            |> List.sortByDescending score
            |> List.tryHead

        cataProduct fBought fMade


let label = Products.Bought {name = "label"; weight = 1; vendor = Some "ACME"}

let bottle = Products.Bought {name = "bottle"; weight = 2; vendor = Some "ACME"}

let formulation = Products.Bought {name = "formulation"; weight = 3; vendor = None}

let shampoo =
    Products.Made {name = "shampoo"; weight = 10; components =
    [
        {qty = 1; product = formulation}
        {qty = 1; product = bottle}
        {qty=2; product = label}
    ]}

let twoPack =
    Products.Made {name = "twoPack"; weight = 5; components =
    [
        {qty = 2; product = shampoo}
    ]}

label |> Products.productWeight
shampoo |> Products.productWeight
twoPack |> Products.productWeight

label |> Products.mostUsedVendor
formulation |> Products.mostUsedVendor
shampoo |> Products.mostUsedVendor
twoPack |> Products.mostUsedVendor
