type Book = {title: string; price: decimal}

type ChocolateType = Dark | Milk | SeventyPercent
type Chocolate = {chocType: ChocolateType; price: decimal}

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

let wolfHall = {title = "Wolf Hall"; price=20m}

let yummyChoc = {chocType=Milk; price=5m}

let birthdayPresent = WithACard (Wrapped (Book wolfHall, HappyBirthday), "Happy Birthday")

let christmasPresent = Wrapped (Boxed (Chocolate yummyChoc), HappyHolidays)

module ``A basic recursive type`` =
    let rec description gift =
        match gift with
        | Book book ->
            sprintf "'%s'" book.title
        | Chocolate choc ->
            sprintf "%A chocolate" choc.chocType
        | Wrapped (innerGift,style) ->
            sprintf "%s wrapped in %A paper" (description innerGift) style
        | Boxed innerGift ->
            sprintf "%s in a box" (description innerGift)
        | WithACard (innerGift,message) ->
            sprintf "%s with a card saying '%s'" (description innerGift) message

    let rec totalCost gift =
        match gift with
        | Book book ->
            book.price
        | Chocolate choc ->
            choc.price
        | Wrapped (innerGift,style) ->
            (totalCost innerGift) + 0.5m
        | Boxed innerGift ->
            (totalCost innerGift) + 1.0m
        | WithACard (innerGift,message) ->
            (totalCost innerGift) + 2.0m

    let rec whatsInside gift =
        match gift with
        | Book book ->
            "A book"
        | Chocolate choc ->
            "Some chocolate"
        | Wrapped (innerGift,style) ->
            whatsInside innerGift
        | Boxed innerGift ->
            whatsInside innerGift
        | WithACard (innerGift,message) ->
            whatsInside innerGift

module ``A basic recursive type using Catamorphism`` =
    let rec cataGift fBook fChocolate fWrapped fBoxed fCard gift : 'r =
        let recurse = cataGift fBook fChocolate fWrapped fBoxed fCard
        match gift with
        | Book book -> fBook book
        | Chocolate choc -> fChocolate choc
        | Wrapped (gift,style) -> fWrapped (recurse gift,style)
        | Boxed gift -> fBoxed (recurse gift)
        | WithACard (gift,message) -> fCard (recurse gift,message)

    let totalCost gift =
        let fBook (book:Book) = book.price
        let fChocolate (choc:Chocolate) = choc.price
        let fWrapped (innerCost,style) = innerCost + 0.5m
        let fBoxed innerCost = innerCost + 1.0m
        let fCard (innerCost,message) = innerCost + 2.0m
        cataGift fBook fChocolate fWrapped fBoxed fCard gift

    let description gift =
        let fBook (book:Book) = sprintf "'%s'" book.title
        let fChocolate (choc:Chocolate) = sprintf "%A chocolate" choc.chocType
        let fWrapped (innerText,style) = sprintf "%s wrapped in %A paper" innerText style
        let fBoxed innerText = sprintf "%s in a box" innerText
        let fCard (innerText,message) = sprintf "%s with a card saying '%s'" innerText message
        cataGift fBook fChocolate fWrapped fBoxed fCard gift

    let handleContents fBook fCocolate =
        let fWrapped (gift,style) = gift
        let fBoxed gift = gift
        let fCard (gift,message) = gift
        cataGift fBook fCocolate fWrapped fBoxed fCard

    let whatsinside =
        handleContents
            (fun book -> "The book you wanted")
            (fun choc -> "Your fave chocolate")

let test f =
    f birthdayPresent |> printfn "birthdayPresent: %A"
    f christmasPresent |> printfn "christmasPresent: %A"

;;

test ``A basic recursive type``.description
test ``A basic recursive type using Catamorphism``.description

test ``A basic recursive type``.totalCost
test ``A basic recursive type using Catamorphism``.totalCost

test ``A basic recursive type``.whatsInside
test ``A basic recursive type using Catamorphism``.whatsinside

module Mapping =
    type GiftMinusChocolate =
        | Book of Book
        | Apology of string
        | Wrapped of GiftMinusChocolate * WrappingPaperStyle

    let removeChocolate =
        let fBook (book: Book) = Book book
        let fChocolate (choc:Chocolate) = Apology "sorry I ate your chocolate"
        let fWrapped (gift,style) = Wrapped (gift,style)
        let fBoxed gift = gift
        let fCard (gift,message) = gift
        ``A basic recursive type using Catamorphism``.cataGift fBook fChocolate fWrapped fBoxed fCard

test Mapping.removeChocolate

let deepCopy = ``A basic recursive type using Catamorphism``.cataGift Book Chocolate Wrapped Boxed WithACard

test deepCopy

let upgradeChocolate =
    let fChocolate (choc:Chocolate) = Chocolate {choc with chocType = SeventyPercent}
    ``A basic recursive type using Catamorphism``.cataGift Book fChocolate Wrapped Boxed WithACard

test upgradeChocolate
