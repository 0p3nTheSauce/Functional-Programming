type MyList<'data> = 
    | Empty
    | Element of 'data * MyList<'data>

type NoteOrItem<'data> =
    | Sorry
    | Item of 'data

let push v l = Element (v, l)

let listString list =
    let rec toString =
        function
            | Empty -> " "
            | Element (v, Empty) -> string v
            | Element (v, nested) -> string v + ", " + toString nested
    "[" + toString list + "]"

let reverse list =
    let rec reversal output =
        function
            | Empty ->
                output
            | Element (x, remaining) ->
                reversal (push x output) remaining
    reversal Empty list

let testEmpty: MyList<int> = Empty

let pop = 
    function 
        | Empty -> Sorry
        | Element (v, Empty) -> Item (Item v, Item Empty)
        | Element (v, nested) -> Item (Item v, Item nested)

// listString <| (Empty |> push 99 |> push 12 |> push 7)
// pop (Empty |> push 99 |> push 12 |> push 7)
// pop testEmpty


let rec length =
    function 
        | Empty -> 0
        | Element (v, Empty) -> 1
        | Element (v, nested) -> 1 + length nested

// length <| (Empty |> push 99 |> push 12 |> push 7)

let filter condition list = 
    let rec inFilter condition output = 
        function 
            | Empty -> 
                output
            | Element (x, remaining) ->
                if condition x then 
                    inFilter condition (push x output) remaining
                else 
                    inFilter condition output remaining
    inFilter condition Empty list |> reverse 


let myMap = Empty |> push 9 |> push 2 |> push 7 |> push 4 |> push 1

// listString myMap
// filter (fun x -> x % 2 = 0) myMap |> listString
// filter (fun v -> v < 4) myMap |> listString
// filter (fun w -> w > 10) myMap |> listString

type AlwaysList<'data> = 
    | Single of 'data
    | Value of 'data * AlwaysList<'data>

let rec choose condition = 
    function 
        | Empty -> Sorry
        | Element (x, nested) ->
            if condition x then 
                Item x 
            else 
                choose condition nested
        
// choose (fun x -> x % 2 = 0) myMap
// choose (fun v -> v < 4) myMap
// choose (fun w -> w > 10) myMap

let partition condition list =
    let rec inPartition condition (outMatch, outNotMatch) = 
        function
            | Empty ->
                (outMatch, outNotMatch)
            | Element (x, remaining) ->
                if condition x then 
                    inPartition condition (push x outMatch, outNotMatch) remaining 
                else
                    inPartition condition (outMatch, push x outNotMatch) remaining
    inPartition condition (Empty, Empty) list

// let myMap = Empty |> push 9 |> push 2 |> push 7 |> push 4 |> push 1
// partition (fun x -> x % 2 = 0) myMap |> (fun (a, b) -> a) |>listString
// partition (fun x -> x % 2 = 0) myMap |> (fun (a, b) -> b) |>listString
// partition (fun v -> v < 4) myMap |> (fun (a, b) -> a) |> listString
// partition (fun v -> v < 4) myMap |> (fun (a, b) -> b) |> listString
// partition (fun w -> w > 10) myMap |> (fun (a, b) -> a) |> listString
// partition (fun w -> w > 10) myMap |> (fun (a, b) -> b) |> listString

let flatten list = 
    let rec transfer outputList = 
        function
            | Empty ->
                outputList
            | Element (v, remaining) ->
                transfer (push v outputList) remaining

    let rec innerFlatten outputList = 
        function
            | Empty -> 
                outputList
            | Element (aList, remaining) ->
                innerFlatten (transfer outputList aList) remaining
    innerFlatten Empty list |> reverse

let first = Empty |> push 6 |> push 3
let second = Empty |> push 71
let third = Empty |> push 9910 |> push 87 |> push 6

// flatten (Empty |> push third |> push second |> push first)
//     |> listString

type ExcuseOrValue<'data, 'reason> = 
    | Excuse of 'reason
    | Value of 'data

let andThen nextStep = 
    function 
        | Excuse reason -> Excuse reason
        | Value v -> Value (nextStep v)

let addBag price = price + 0.28

let discount price = 
    if price > 1000.0 then 
        price * 0.95
    else    
        price

let calculateTax rate price = 
    if rate <= 0.0 then 
        Excuse "Tax rate cannot be <= 0.0"
    else
        Value (rate * price)

let checkout rate = 
    calculateTax rate >> (andThen discount) >> (andThen addBag)

checkout 1.15 1698.1