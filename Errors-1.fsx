type NoteOrItem<'data> = 
| Sorry
| Item of 'data

let calculateTax rate price = 
    if rate <= 0.0 then 
        Sorry
    else
        Item (rate * price)

let addBag price = price + 0.28

let discount price = 
    if price > 1000.0 then 
        price * 0.95
    else
        price

let checkout rate = 
    calculateTax rate >> discount >> addBag

checkout 1.15 1698.10