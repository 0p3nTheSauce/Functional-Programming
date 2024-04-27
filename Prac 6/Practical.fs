type PartOfCommand =
| Word of string
| Integer of int
| Negative
| Space

type List<'a> =
| EndOfList
| V of 'a * List<'a>

type Maybe<'data> =
| Just of 'data
| Nothing

type Xcord = 
| X of int

type Ycord = 
| Y of int

type Width = 
| W of int 

type Height =
| H of int 

type Screen =
| Screen of Width * Height

type Window =
| Window of Xcord * Ycord * Width * Height

type Command =
| Open of Xcord * Ycord * Width * Height
| Close of Xcord * Ycord
| Resize of Xcord * Ycord * Width * Height
| Move of Xcord * Ycord * Width * Height
| Quit

let stringFold f initial (s : string) =
    Seq.fold (fun state ch -> f state (string ch)) initial s

let stringReverse = stringFold (fun state item -> item + state) ""

let stringToList =
    stringReverse
    >> stringFold (fun state item -> V (item, state)) EndOfList

let listFold f initial =
    let rec listFold state =
        function
        | EndOfList -> state
        | V (item, rest) -> listFold (f state item) rest
    listFold initial

let listReverse list =
    listFold (fun state item -> V (item, state)) EndOfList list

let listMap f list =
    listReverse list
    |> listFold (fun state item -> V (f item, state)) EndOfList

let rec removeSpaces =
    function
    | EndOfList -> EndOfList
    | V (Space, rest) -> removeSpaces rest
    | V (item, rest) -> V (item, removeSpaces rest)

let toPartOfCommand s =
  if s = " " then
    Space
  elif s = "0" then
    Integer 0
  elif s = "1" then
    Integer 1
  elif s = "2" then
    Integer 2
  elif s = "3" then
    Integer 3
  elif s = "4" then
    Integer 4
  elif s = "5" then
    Integer 5
  elif s = "6" then
    Integer 6
  elif s = "7" then
    Integer 7
  elif s = "8" then
    Integer 8
  elif s = "9" then
    Integer 9
  elif s = "-" then
    Negative
  else
    Word s

let rec merge =
    function
    | EndOfList -> EndOfList
    | V (Word a, V (Word b, rest)) ->
        merge (V (Word (a + b), rest))
    | V (Integer a, V (Integer b, rest)) ->
        merge (V (Integer (a * 10 + b), rest))
    | V (Negative, V (Integer a, rest)) ->
        merge (V (Integer (-a), rest))
    | V (x, rest) ->
        V (x, merge rest)

let rec toCommand =
    function
    | EndOfList -> Nothing
    | V (Word "QUIT", EndOfList) ->
      Just <| Quit
    | V (Word "CLOSE", V (Integer a, V (Integer b, EndOfList)))  ->
        Just <| Close (X a, Y b)
    | V (Word "OPEN", V (Integer a, V (Integer b, V (Integer c, V (Integer d, EndOfList))))) ->
        Just <| Open (X a, Y b, W c, H d)
    | V (Word "RESIZE", V (Integer a, V (Integer b, V (Integer c, V (Integer d, EndOfList))))) ->
        Just <| Resize (X a, Y b, W c, H d)
    | V (Word "MOVE", V (Integer a, V (Integer b, V (Integer c, V (Integer d, EndOfList))))) ->
      Just <| Move (X a, Y b, W c, H d)
    | _ -> Nothing
    
let parse = 
    stringToList
    >> listMap toPartOfCommand
    >> merge
    >> removeSpaces
    >> toCommand

let rec readCommand () =
    System.Console.ReadLine()
    |> parse
    |> (function 
            | Just x ->
                x
            | Nothing -> 
                readCommand()
        )

let push item list = 
    V (item, list)

let xCordToString =
    function
    | X digit ->
        string digit

let yCordToString =
    function
    | Y digit ->
        string digit

let widthToString =
    function
    | W digit ->
        string digit

let heightToString =
    function
    | H digit ->
        string digit

let windowToString =
    function
    | Window (x,y,w,h) ->
        "x: " + xCordToString x +
        " y: " + yCordToString y +
        " w: " + widthToString w +
        " h: " + heightToString h
    | _ ->
        ""

let rec displayWindowList =
// display a list of windows as strings underneath each other
    function
    | EndOfList ->
        ""
    | V (w, rest)->
        windowToString w + "\n" + displayWindowList rest

let createScreen w h =
//creates a screen
    if w >= 1 && w <= 1000000 && h >= 1 && h >= 1000000 then 
        let screen = Screen (W w, H h)
        Just screen
    else
        Nothing


// let rec windowString windowList =
//   match windowList with
//   | EndOfList -> ""
//   | V ( (* some pattern to match whatever your data looks like *) , rest) ->
//       $"(* string form of the element in the list *)\n" + windowString rest

// let writeWindows windowList =
//   System.Console.WriteLine (windowString windowList)
//   windowList

let accessX =
//access the int of X of int
  function
  | X i ->
    i

let accessY =
  function 
  | Y i ->
    i

let accessW =
  function 
  | W i ->
    i 

let accessH =
  function 
  | H i ->
    i

let windowContainsPixel (Window (xi,yi,wi,hi)) (xCord, yCord) =
// identifies a window containing the given pixel
  let x = accessX xi
  let y = accessY yi
  let w = accessW wi
  let h = accessH hi
  let xcord = accessX xCord
  let ycord = accessY yCord
  //the right coordinate will be (x+w-1) because the width included the starting coordinate
  //same logic for the bottom coordinate (y+h-1)
  xcord >= x && xcord <= (x+w-1) && ycord >= y && ycord <= (y+h-1) 

let screenContainsPixel (Screen (wi,hi)) (xcord,ycord) =
//checks if screen contains the pixel
  let w = accessW wi
  let h = accessH hi
  let xcord = accessX xcord
  let ycord = accessY ycord
  xcord >= 0 && xcord <= w && ycord >= 0 && ycord <= h

let removeFrom list pixel =
// removes the window containing the pixel from the list 
  let rec inRemove newList oldList pixel =
    (function
    | EndOfList ->
      newList
    | V (element, EndOfList) ->
      if windowContainsPixel element pixel then 
        newList
      else 
        push element newList
    | V (element, rest) ->
      if windowContainsPixel element pixel then 
        inRemove newList rest pixel 
      else 
        inRemove (push element newList) rest pixel
    ) oldList
  inRemove EndOfList list pixel
  |> listReverse

let getCornersOfWindow (Window (xi,yi,wi,hi)) =
//returns the coordinates of the four corners of an input window
  let x = accessX xi
  let y = accessY yi
  let w = accessW wi
  let h = accessH hi
  //for a window of (1,1,2,2) the top left pixel is (1,1), the width is 2 and the height is 2
  //the top right pixel is (2,1)
  //bottom left pixel is (1,2) etc
  let topLeft = (xi, yi)
  let topRight = (X (x+w-1), yi) 
  let bottomLeft = (xi, Y(y+h-1))
  let bottomRight = (X (x+w-1), Y(y+h-1))
  (topLeft, topRight, bottomLeft, bottomRight)

let cornersInside window1 window2 =
//checks if the corners of window 2, are inside window1
  let (tl2,tr2,bl2,br2) = getCornersOfWindow window2
  windowContainsPixel window1 tl2 ||
  windowContainsPixel window1 tr2 ||
  windowContainsPixel window1 bl2 ||
  windowContainsPixel window1 br2

let windowInside screen window = 
//checks if a window is inside the screen
  let (tlw,trw,blw,brw) = getCornersOfWindow window
  screenContainsPixel screen tlw &&
  screenContainsPixel screen trw &&
  screenContainsPixel screen blw &&
  screenContainsPixel screen brw 

let xOf (X x, Y y) =
//access the x coordinate of a coordinate tuple
  x 

let yOf (X x, Y y) =
//access the x coordinate of a coordinate tuple
  y

let intersectingLines (a,b,c,d) =
//checks if two lines a-b and c-d are overlapping 
//where a-b is vertical and c-d is horizontal
//and comparing the vertical line to the horizontal line 
  (xOf c) <= (xOf a) && (xOf d) >= (xOf a) &&
  (yOf c) >= (yOf a) && (yOf c) <= (yOf b)

let intersecting window1 window2 =
//checks if the edges of two windows are intersecting
  let (tl1,tr1,bl1,br1) = getCornersOfWindow window1
  let (tl2,tr2,bl2,br2) = getCornersOfWindow window2
  intersectingLines (tl1,bl1,tl2,tr2) || intersectingLines (tl2,bl2,tl1,tr1)
  //the order of the windows should not matter
  
let overLapping window1 window2 = 
//checks if two windows are overlapping
  cornersInside window1 window2 || 
  cornersInside window2 window1 ||
  intersecting window1 window2

let addTo list element =
// adds the speicified element to the list 
  let rec inAddTo newList oldList newElement originalList =
    (function
    | EndOfList ->
      push newElement newList
    | V (element, rest) ->
      if overLapping element newElement then
        originalList |> listReverse 
      else 
        inAddTo (push element newList) rest newElement originalList
    ) oldList
  inAddTo EndOfList list element list
  |> listReverse

let rec containsOverlap list window =
//checks if there is an overlap between the window and the other windows in the list
  (function
  | EndOfList ->
    false
  | V (element, rest) ->
    if overLapping element window then 
      true
    else 
      containsOverlap rest window  
  ) list  

let alterWindowList list pixel newAttributes =
// alters a window in a window list 
  let rec inAlter newlist oldList original pixel newAttributes =
    (function
    | EndOfList ->
      newlist
    | V (window, rest) ->
      if windowContainsPixel window pixel then  
        let newWindow = Window newAttributes
        let listWithoutOld = removeFrom original pixel 
        if containsOverlap listWithoutOld newWindow then 
          original |> listReverse
        else 
          inAlter (push newWindow newlist) rest original pixel newAttributes
      else 
        inAlter (push window newlist) rest original pixel newAttributes
    ) oldList
  inAlter EndOfList list list pixel newAttributes 
  |> listReverse

let rec getWindow list pixel =
//returns the window that contains the pixel
  (function
  | EndOfList ->
    Nothing
  | V (window, rest) ->
    if windowContainsPixel window pixel then
      Just window
    else 
      getWindow rest pixel
  ) list

let getAvailableDistanceRight window windowlist screen = 
//returns the available distance to the right before a window hits another window or edge 
//of the screen 
  let rec inGetDistanceRight (Window (X x, Y y, W w, H h)) windowlist screen distance =
    let newWindow = Window (X (x+1), Y y, W w, H h)
    let windowListMinus1 = removeFrom windowlist (X x, Y y)
    if windowInside screen newWindow && not <| containsOverlap windowListMinus1 newWindow then
      let newWindowList = addTo windowListMinus1 newWindow
      inGetDistanceRight newWindow newWindowList screen (distance+1)
    else 
      distance
  inGetDistanceRight window windowlist screen 0

let getAvailableDistanceLeft window windowlist screen = 
//returns the available distance to the left before a window hits another window or edge 
//of the screen 
  let rec inGetDistanceLeft (Window (X x, Y y, W w, H h)) windowlist screen distance =
    let newWindow = Window (X (x-1), Y y, W w, H h)
    let windowListMinus1 = removeFrom windowlist (X x, Y y)
    if windowInside screen newWindow && not <| containsOverlap windowListMinus1 newWindow then
      let newWindowList = addTo windowListMinus1 newWindow
      inGetDistanceLeft newWindow newWindowList screen (distance+1)
    else 
      distance
  inGetDistanceLeft window windowlist screen 0

let getAvailableDistanceUp window windowlist screen = 
//returns the available distance to the up before a window hits another window or edge 
//of the screen 
  let rec inGetDistanceUp (Window (X x, Y y, W w, H h)) windowlist screen distance =
    let newWindow = Window (X x, Y (y-1), W w, H h)
    let windowListMinus1 = removeFrom windowlist (X x, Y y)
    if windowInside screen newWindow && not <| containsOverlap windowListMinus1 newWindow then
      let newWindowList = addTo windowListMinus1 newWindow
      inGetDistanceUp newWindow newWindowList screen (distance+1)
    else 
      distance
  inGetDistanceUp window windowlist screen 0

let getAvailableDistanceDown window windowlist screen = 
//returns the available distance to the down before a window hits another window or edge 
//of the screen 
  let rec inGetDistanceDown (Window (X x, Y y, W w, H h)) windowlist screen distance =
    let newWindow = Window (X x, Y (y+1), W w, H h)
    let windowListMinus1 = removeFrom windowlist (X x, Y y)
    if windowInside screen newWindow && not <| containsOverlap windowListMinus1 newWindow then
      let newWindowList = addTo windowListMinus1 newWindow
      inGetDistanceDown newWindow newWindowList screen (distance+1)
    else 
      distance
  inGetDistanceDown window windowlist screen 0

let touchingRL (Window (X x, Y y, W w, H h)) window2 =
//checks if two windows are touching right side to left side
  overLapping (Window (X (x+1), Y y, W w, H h)) 

let touchingLR (Window (X x, Y y, W w, H h)) window2 =
//checks if two windows are touching left side to right side 
  overLapping (Window (X (x-1), Y y, W w, H h)) 

let touchingTB (Window (X x, Y y, W w, H h)) window2 =
//checks if two windows are touching top to bottom
  overLapping (Window (X x, Y (y-1), W w, H h)) 

let touchingBT (Window (X x, Y y, W w, H h)) window2 =
//checks if two windows are touching bottom to top
  overLapping (Window (X x, Y (y+1), W w, H h)) 

let moveWindowRight (Window (X x, Y y, W w, H h)) dx =
// move window to the right by dx
  Window (X (x+dx), Y y, W w, H h)
   
let moveWindowLeft (Window (X x, Y y, W w, H h))  dx =
// move window to the left by dx
  Window (X (x-dx), Y y, W w, H h)

let moveWindowUp (Window (X x, Y y, W w, H h)) dy =
// move window up by dy
  Window (X x, Y (y-dy), W w, H h)

let moveWindowDown (Window (X x, Y y, W w, H h)) dy =
// move window down by dy
  Window (X x, Y (y+dy), W w, H h)

let moveWindowsRight windowlist dx = 
//move all windows in the list to the right by dx
  let rec inMWRight newList oldList dx =
    (function
    | EndOfList ->
      newList
    | V (window, rest) ->
      inMWRight (push (moveWindowRight window dx) newList) rest dx 
    ) oldList
  inMWRight EndOfList windowlist dx 

let closeWindow (xcord, ycord) windowlist = 
//removes the specified window from the window list
  removeFrom windowlist (xcord,ycord)

let openWindow (xcord,ycord,width,height) windowlist screen = 
//adds the specified window to the windowlist if there is space
  let newWindow = Window (xcord,ycord,width,height)
  if windowInside screen newWindow then 
    addTo windowlist newWindow
  else 
    windowlist

let resizeWindow (xcord,ycord,width,height) windowlist screen =
//resizes the window specified by (xcord,ycord)
  if windowInside screen (Window (xcord,ycord,width,height)) then
    alterWindowList windowlist (xcord,ycord) (xcord,ycord,width,height)
  else 
    windowlist



// let processCommand (width, height) windowlist command =
//   (function
//   | Quit ->
//     windowlist
//   | Close (xcord, ycord) ->
//     removeFrom windowlist windowContainsPixel (xcord, ycord)
//   | 
//   ) command

// let rec simulate screenWidth screenHeight =
//   let rec helper windowList =
//     readCommand ()
//     |> ( function
//       | (* some pattern to match your quit command *) ->
//         ()
//       | otherCommand ->
//         processCommand (screenWidth, screenHeight) windowList otherCommand
//         |> writeWindows
//         |> helper
//     )
//   helper EndOfList

let testX = X 1
let testY = Y 1
let testW = W 4
let testH = H 4

let sampleX = X 2
let sampleY = Y 2

//test screen
let testScreen = Screen (W 10, H 10)

//testwindows
let testWindow = Window (testX, testY, testW, testH)

let testOutOfScreen = Window (X -1, Y 1, W 1, H 1)

let win = Window (X 6, Y 1, W 4, H 4)

let testOverlapWin = Window (X 4, Y 2, W 3, H 2)

//OverLapTestWindow (intersecting)
let oltw1 = Window (X 4, Y 6, W 2, H 6)
let oltw2 = Window (X 2, Y 8, W 6, H 2)

//OverLapTestLines
let a = (X 4, Y 6)
let b = (X 4, Y 11)
let c = (X 2, Y 8)
let d = (X 7, Y 8)

let testList = EndOfList |> push testWindow |> push win |> push oltw1

// let testIntList = Empty |> push 5 |> push 4 |> push 3 

// let testCommand = Open (testX, testY, testW, testH)

//type State =
//    | Inuse
//    | Notinuse


// let io state = 
//    (function
//        | Inuse ->
//            System.Console.WriteLine("give me a number ")

//            let number = System.Console.ReadLine()

//            let num = int number

//            let rec printnum num =
//                if num = 0 then 
//                    System.Console.WriteLine(string num)
//                else 
//                    System.Console.WriteLine(string num)
//                    printnum(num - 1)

//            printnum num
//        | Notinuse ->
//            System.Console.WriteLine("thanks")
//            ()
//    ) state

//let input = io Inuse 



// let (tl1,tr1,bl1,br1) = getCornersOfWindow (testWindow)

// let command = readCommand ()

getAvailableDistanceDown win testList testScreen 

// screenContainsPixel testScreen (X 0, Y -1)

// windowInside testScreen (Window (X 0, Y -1, W 4, H 4))