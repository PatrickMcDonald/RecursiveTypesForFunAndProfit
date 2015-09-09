type Book = {
    title: string
    price: decimal
    }

type ChocolateType =
    | Dark
    | Milk
    | SeventyPercent

type Chocolate = {
    chocType: ChocolateType
    price: decimal
    }

type WrappingPaperStyle =
    | HappyBirthday
    | HappyHolidays
    | SolidColor

type Gift =
    | Book of Book
    | Chocolate of Chocolate
    | Wrapped of Gift * WrappingPaperStyle
    | Boxed of Gift
    | WithACard of Gift * message:string

// A Book
let wolfHall = {title="Wolf Hall"; price=20m}
// A Chocolate
let yummyChoc = {chocType=SeventyPercent; price=5m}
// A Gift
let birthdayPresent = WithACard (Wrapped (Book wolfHall, HappyBirthday), "Happy Birthday")
// A Gift
let christmasPresent = Wrapped (Boxed (Chocolate yummyChoc), HappyHolidays)

let rec cataGift fBook fChocolate fWrapped fBox fCard gift :'r =
    let recurse = cataGift fBook fChocolate fWrapped fBox fCard
    match gift with
    | Book book -> fBook book
    | Chocolate choc -> fChocolate choc
    | Wrapped (gift,style) -> fWrapped (recurse gift) style
    | Boxed gift -> fBox (recurse gift)
    | WithACard (gift,message) -> fCard (recurse gift) message

let totalCostUsingCata =
    let fBook (book:Book) =
        book.price
    let fChocolate (choc:Chocolate) =
        choc.price
    let fWrapped innerCost style =
        innerCost + 0.5m
    let fBox innerCost =
        innerCost + 1.0m
    let fCard innerCost message =
        innerCost + 2.0m
    // call the catamorphism
    cataGift fBook fChocolate fWrapped fBox fCard

let deeplyNestedBox depth =
    let rec loop depth boxSoFar =
        match depth with
        | 0 -> boxSoFar
        | n -> loop (n-1) (Boxed boxSoFar)
    loop depth (Book wolfHall)

deeplyNestedBox 5

deeplyNestedBox 10

deeplyNestedBox 10 |> totalCostUsingCata
deeplyNestedBox 100 |> totalCostUsingCata
deeplyNestedBox 1000 |> totalCostUsingCata

[10; 100; 1000]
|> List.map (fun depth -> deeplyNestedBox depth |> totalCostUsingCata)

// stackoverflow deeplyNestedBox 10000 |> totalCostUsingCata
// stackoverflow deeplyNestedBox 100000 |> totalCostUsingCata

let totalCostUsingAcc =
    let rec loop costSoFar gift =
        match gift with
        | Book book ->
            costSoFar + book.price
        | Chocolate choc ->
            costSoFar + choc.price
        | Wrapped (gift,style) ->
            loop (costSoFar + 0.5m) gift
        | Boxed gift ->
            loop (costSoFar + 1.0m) gift
        | WithACard (gift,message) ->
            loop (costSoFar + 2.0m) gift
    loop 0.0m

[10; 100; 1000; 10000; 100000; 1000000]
|> List.map (fun depth -> deeplyNestedBox depth |> totalCostUsingAcc)

let rec foldGift fBook fChocolate fWrapped fBox fCard acc gift : 'r =
    let recurse = foldGift fBook fChocolate fWrapped fBox fCard
    match gift with
    | Book book ->
        fBook acc book
    | Chocolate choc ->
        fChocolate acc choc
    | Wrapped (gift,style) ->
        let newAcc = fWrapped acc style
        recurse newAcc gift
    | Boxed gift ->
        let newAcc = fBox acc
        recurse newAcc gift
    | WithACard (gift,message) ->
        let newAcc = fCard acc message
        recurse newAcc gift

let totalCostUsingFold =
    let fBook acc (book:Book) = acc + book.price
    let fChocolate acc (choc:Chocolate) = acc + choc.price
    let fWrapped acc style = acc + 0.5m
    let fBox acc = acc + 1.0m
    let fCard acc message = acc + 2.0m
    foldGift fBook fChocolate fWrapped fBox fCard 0.0m

[10; 100; 1000; 10000; 100000; 1000000]
|> List.map (fun depth -> deeplyNestedBox depth |> totalCostUsingFold)

let descriptionUsingFold =
    let fBook acc (book:Book) = sprintf "'%s' %s" book.title acc
    let fChocolate acc (choc:Chocolate) = sprintf "%A chocolate %s" choc.chocType acc
    let fWrapped acc style = sprintf "%s wrapped in %A paper" acc style
    let fBox acc = sprintf "%s in a box" acc
    let fCard acc message = sprintf "%s with a card saying '%s'" acc message
    foldGift fBook fChocolate fWrapped fBox fCard ""

birthdayPresent |> descriptionUsingFold
christmasPresent |> descriptionUsingFold

let descriptionUsingFoldWithGenerator =
    let fBook gen (book:Book) =
        gen (sprintf "'%s'" book.title)
    let fChocolate gen (choc:Chocolate) =
        gen (sprintf "%A chocolate" choc.chocType)
    let fWrapped gen style innerText =
        gen (sprintf "%s wrapped in %A paper" innerText style)
    let fBox gen innerText =
        gen (sprintf "%s in a box" innerText)
    let fCard gen message innerText =
        gen (sprintf "%s with a card saying '%s'" innerText message)
    foldGift fBook fChocolate fWrapped fBox fCard id

birthdayPresent |> descriptionUsingFoldWithGenerator
christmasPresent |> descriptionUsingFoldWithGenerator

let rec foldBackGift fBook fChocolate fWrapped fBox fCard gift gen : 'r =
    let recurse = foldBackGift fBook fChocolate fWrapped fBox fCard
    match gift with
    | Book book ->
        gen (fBook book)
    | Chocolate choc ->
        gen (fChocolate choc)
    | Wrapped (gift,style) ->
        recurse gift (fun innerVal -> gen (fWrapped style innerVal))
    | Boxed gift ->
        recurse gift (fun innerVal -> gen (fBox innerVal))
    | WithACard (gift,message) ->
        recurse gift (fun innerVal -> gen (fCard message innerVal))

let descriptionUsingFoldBack gift =
    let fBook (book:Book) =
        sprintf "'%s'" book.title
    let fChocolate (choc:Chocolate) =
        sprintf "%A chocolate" choc.chocType
    let fWrapped style innerText =
        sprintf "%s wrapped in %A paper" innerText style
    let fBox innerText =
        sprintf "%s in a box" innerText
    let fCard message innerText =
        sprintf "%s with a card saying '%s'" innerText message
    foldBackGift fBook fChocolate fWrapped fBox fCard gift id

birthdayPresent |> descriptionUsingFoldBack
christmasPresent |> descriptionUsingFoldBack

