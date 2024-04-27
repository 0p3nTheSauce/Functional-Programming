type NoteOrItem<'data> = 
| Sorry
| Item of 'data

let calculateTax rate price = 
    if rate <= 0.0 then 
        Sorry
    else
        Item (rate * price)

let discount = 
    function
        | Sorry -> Sorry
        | Item p ->
            if p > 1000.0 then 
                Item (p * 0.95)
            else
                Item p

let addBag = 
    function 
        | Sorry -> Sorry
        | Item p -> Item (p + 0.28)

let checkout rate = 
    calculateTax rate >> discount >> addBag

checkout 1.15 1698.1