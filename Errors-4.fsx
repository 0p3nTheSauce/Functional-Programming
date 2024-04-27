type NoteOrItem<'data> =
    | Sorry
    | Item of 'data

let andThen nextStep = 
    function
        | Sorry -> Sorry
        | Item v -> Item (nextStep v)

let addBag price = price + 0.28

let discount price = 
    if price > 1000.0 then 
        price * 0.95
    else    
        price

let calculateTax rate price = 
    if rate <= 0.0 then 
        Sorry
    else
        Item (rate * price)

let checkout rate = 
    calculateTax rate >> (andThen discount) >> (andThen addBag)

checkout -1.15 1698.1