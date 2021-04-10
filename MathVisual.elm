
import String as String
import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events

myShapes model =
    case model.state of
        ExpenseType -> [inputTemplate "Enter: Normal or Recurrent Expense" model.expenseType]
        ExpenseName -> [inputTemplate "Enter: Name of the Expense" model.expenseName]
        ExpenseAmount -> [inputTemplate "Enter: Expense Amount" model.expenseAmount]
        Nday->[inputTemplate "Enter: Day" model.nDay]
        Nmonth->[inputTemplate "Enter: Month" model.nMonth]        
        Nyear->[inputTemplate "Enter: Year" model.nYear]      
        Rday->[inputTemplate "Enter: Day" model.rDay]
        Rmonth->[inputTemplate "Enter: Month" model.rMonth]
        Ryear->[inputTemplate "Enter: Year" model.rYear]
        ChooseCat->[]
        MainMenu  ->
            [
              roundedRect 40 25 5 |> filled (rgb 37 199 107) |> move(-71, 45)
              ,roundedRect 40 25 5 |> outlined (solid 0.5) (rgb 0 0 0) |> move(-71, 45)
              ,roundedRect 40 25 5 |> filled (rgb 37 199 107) |> move(-21.5, 45)
              ,roundedRect 40 25 5 |> outlined (solid 0.5) (rgb 0 0 0) |> move(-21.5, 45)
              ,GraphicSVG.text "Budget" |> filled black  |>move(-107,60) |>scale 0.8
              ,GraphicSVG.text "Expenses" |> filled black  |>move(-50, 60) |>scale 0.8
              ,GraphicSVG.text "20000" |> filled black |>move(-115,50) |>scale 0.7
              ,GraphicSVG.text (String.fromFloat (List.sum (List.map (\x -> count_amount x.expenseList 0) model.categories)) ) |> filled black |>move(-47,50) |>scale 0.7
              ,button "All Expenses" ToCategories (-21,-2.5) 0.6 |> move (0,-45.5)
              ,buttonRN model "Recurrent" ToRExpense (-9,-2.5) 0.4 gray darkRed |> move (80,55)
              ,buttonRN model "Normal" ToNExpense (-7,-2.5) 0.4 darkRed gray |> move (50,55)
              ,button "Add Expense" AddExpense (-21,-2.5) 0.6 |> move (70,-45.5)
              ,button "Recurrent Expenses" ToRE (-22.7,-2.5) 0.48 |> move (-70,-45)
              {-,inputBox (case model.expenseType of
                              "" -> "Expense Type:"
                              _ -> model.expenseType) ToExpenseType (-19,-1.5) 0.48 |> move (65,40)-}
              ,inputBox (case model.expenseName of
                              "" -> "Expense Name:"
                              _ -> model.expenseName) ToExpenseName (-19,-1.5) 0.48 |> move (65,23)
              ,inputBox (case model.expenseAmount of
                              "" -> "Expense Amount:"
                              _ -> model.expenseAmount) ToExpenseAmount (-19,-1.5) 0.48 |> move (65,7)
              ,smallInputBox (case model.nDay of
                              "" -> "DD"
                              _ -> model.nDay) ToNday (-4,-2) 0.48 |> move (44,-9)
              ,smallInputBox (case model.nMonth of
                              "" -> "MM"
                              _ -> model.nMonth) ToNmonth (-5,-2) 0.48 |> move (60,-9)
              ,mediumInputBox (case model.nYear of
                              "" -> "YYYY"
                              _ -> model.nYear) ToNyear (-8,-2) 0.48 |> move (81,-9)
              ,case model.expenseTypeRN of
                               "Recurrent" -> recurrentInputs model                      
                               "Normal" -> group[]
                               otherwise   -> group[]
              ,buttonCat (case model.catName of
                              "" -> "Category" 
                              _ -> model.catName) ToChooseCat (-24,-3) 0.7 |> move (65, 40)
              , case model.currentExpenseName of
                                  "" -> group []
                                  _ -> chooseCat

              ]
        Categories  ->
            [ text "Categories"
                  |> centered
                  |> filled black
                  |> move (0,45)
              , button "Back" ToMain (-12,-4) 1 |> move (100,-65) |> scale 0.75
              , let scale = ((length model.categories)-1)/2
                in createPartialCategories model.categories (length model.categories) -(scale * 175/(length model.categories))
            ]
        RExpenses  ->
            [
              roundedRect 100 100 5
                |> filled lightBlue
                |> move (-40, 0)
              ,
              roundedRect 100 100 5
                |> outlined (solid 0.5) black
                |> move (-40, 0)
              ,
              buildListOfRe model
                |> move model.scrollPos
              ,
              rect 100 15
                |> filled white
                |> move(-40, 57)
              ,
              rect 100 20
                |> filled white
                |> move (-40, -60)
              
              --,
              --buildReExpense dummyExpense
              --buildReExpense (Recurrent "Loans" 1000 {day = 21, month = 3, year = 2} {day = 20, month = 0, year = 1})
                -- |> scale 0.5
                -- |> scaleX 0.9
                -- |> move (-66, 28)
              ,
              button "Back" ToMain (-7,-2) 0.5
                |> move (60, -40)
              ,
              scrollBar model
              --,
              --html 0 0 (Html.input [ Attributes.placeholder "Text to reverse", Attributes.value model.content, Events.onInput Change ])
              ,
              text (dateToString model.date)
                |> filled black
                |> move(65, 50)
                |> scale 0.5
              ,
              button "Next Day" NextDay (-10, 0) 0.5
                |> move (59, 0)
              ,
              buildDueExpenseList model
              -- ,
              -- text (String.fromFloat model.budget)
                -- |> filled black
                -- |> move (40, 0)
            ]
        Help  ->
            [ text "Help"
                  |> centered
                  |> filled black
            ]
        Settings  ->
            [ text "Settings"
                  |> centered
                  |> filled black
            ]
        FullCategory category ->
            [ 
              roundedRect 130 110 5 |> filled (rgb 150 200 250) |> move (-25,-30)
              , roundedRect 130 110 5 |> outlined (solid 0.5) black |> move (-25,-30)
              , triangle 10 |> outlined (solid 0.5) black |> rotate (degrees 30) |> move (55,-15)
              , triangle 10 |> outlined (solid 0.5) black |> rotate (degrees -30) |> move (55,0) 
              , triangle 10 |> filled (rgb 0 200 100) |> rotate (degrees 30) |> move (55,-15) |> notifyTap ScrollUp
              , triangle 10 |> filled (rgb 0 200 100) |> rotate (degrees -30) |> move (55,0) |> notifyTap ScrollDown
              , createFullList category.expenseList 30 |> move (0,model.scroll) -- add function here instead of group
              , rect 500 500 |> filled white |> move (0,275)
              , text category.name
                  |> centered
                  |> filled black
                  |> move (0,45)
              , group 
                  [
                   text "Name" |> filled black |> move (-145,50)
                   , text "Amount" |> filled black |> move (-80,50)
                   , text "Date" |> filled black |> move (10,50)
                  ] |> scale 0.6
              , button "Back" ToCategories (-12,-4) 1 |> move (100,-65) |> scale 0.75
            ]



chooseCat = group[roundedRect 55 60 5 |> filled white |> move (65,0),
                   roundedRect 55 60 5 |> outlined (solid 2) green|> move (65,0),
                   text "Heathcare"  |> filled black |>scale 0.7 |> move(45,21) |>notifyTap (ToCurrCategory healthCare),
                   text "Housing"  |> filled black |>scale 0.7 |> move(45,10) |>notifyTap (ToCurrCategory housing),
                   text "Food"  |> filled black |>scale 0.7 |> move(45,-1) |>notifyTap (ToCurrCategory food),
                   text "Transportation"  |> filled black |>scale 0.7 |> move(40,-12) |>notifyTap (ToCurrCategory transportation),
                   text "Utilities"  |> filled black |>scale 0.7 |> move(45,-24) |>notifyTap (ToCurrCategory utilities)]

dummyExpense : Expenses
dummyExpense = 
  let
    autoPayment = {day = 3, month = 0, year = 0}
  in
    Recurrent "Loans" 1000 autoPayment autoPayment


didType keys letter = 
  if keys (Key letter) == JustDown then
    if keys Shift == JustDown || keys Shift == Down then
      [String.toUpper letter]
    else
      [letter]
  else
    []



recurrentInputs model = group[smallInputBox (case model.rDay of
                              "" -> "DD"
                              _ -> model.rDay) ToRday (-4,-2) 0.48 |> move (44,-25)
                        ,smallInputBox (case model.rMonth of
                                        "" -> "MM"
                                        _ -> model.rMonth) ToRmonth (-5,-2) 0.48 |> move (60,-25)
                        ,mediumInputBox (case model.rYear of
                                        "" -> "YYYY"
                                        _ -> model.rYear) ToRyear (-8,-2) 0.48 |> move (81,-25)]


createExpenseFromInput model = case model.expenseTypeRN of
                                                  "Recurrent" -> Recurrent model.expenseName (Maybe.withDefault 0 (String.toFloat model.expenseAmount)) {day = (fromString model.nDay) , month = (fromString model.nMonth) , year = (fromString model.nYear)} {day = (fromString model.rDay) , month = (fromString model.rMonth) , year = (fromString model.rYear)} 
                                                  "Normal" -> Normal model.expenseName (Maybe.withDefault 0 (String.toFloat model.expenseAmount)) {day = (fromString model.nDay) , month = (fromString model.nMonth) , year = (fromString model.nYear)} 
                                                  otherwise -> Normal "Fake" 0.0 {day = 21, month = 3, year = 2}
                               
fromString number =  Maybe.withDefault 0 (String.toInt number)
 
inputTemplate someText inputString = group[GraphicSVG.text someText|> centered |> filled black |>move(0,30) |> scale 0.9
                      ,roundedRect 120 20 5|> outlined (solid 1) green |>move(-20,-5)
                      ,GraphicSVG.text inputString |>filled black |>move(-40,-10)
                      ,button "Done" ToMain (-12,-4) 1 |> move (100,7) |> scale 0.75
                      ,button "Clear" ClearInput (-12,-4) 1 |> move (100,-17) |> scale 0.75] 

--dummyExpense : Expenses
--dummyExpense = Recurrent "Loans" 1000 {day = 21, month = 3, year = 2} {day = 20, month = 0, year = 1}

dummyExpenseList = [ Recurrent "Loans" 1000 {day = 21, month = 3, year = 2} {day = 20, month = 0, year = 1},  Recurrent "Loans" 103300 {day = 21, month = 3, year = 2} {day = 20, month = 0, year = 1},
 Recurrent "Loans" 1200 {day = 21, month = 3, year = 2} {day = 20, month = 0, year = 1}]

--dummyExpenseList1 : List Expenses
--dummyExpenseList1 = []

length list = case list of
                [] -> 0
                x::xs -> 1 + length xs 

createFullList expenseList ypos = case expenseList of
                          [] -> group []
                          x::xs -> case x of
                                     Normal name amount date -> group 
                                                           [
                                                             text name |> filled black |>  move (-170,ypos) |> scale 0.5
                                                             , text ("$"++(String.fromFloat amount)) |> filled black |>  move (-90,ypos) |> scale 0.5
                                                             , text ((String.fromInt date.day) ++ "/" ++ (String.fromInt date.month) ++ "/" ++ (String.fromInt date.year)) |> filled black |>  move (0,ypos) |> scale 0.5
                                                             , createFullList xs (ypos-15)
                                                           ]
                                     Recurrent name amount date extra -> group 
                                                           [
                                                             text name |> filled black |>  move (-170,ypos) |> scale 0.5
                                                             , text ("$"++(String.fromFloat amount)) |> filled black |>  move (-90,ypos) |> scale 0.5
                                                             , text ((String.fromInt date.day) ++ "/" ++ (String.fromInt date.month) ++ "/" ++ (String.fromInt date.year)) |> filled black |>  move (0,ypos) |> scale 0.5
                                                             , createFullList xs (ypos-15)
                                                           ]                       
                                   


createPartialCategories categoryList numberOfCategories position = case categoryList of 
                                                            [] -> group []
                                                            x::xs -> group 
                                                                          [
                                                                           group [roundedRect ((175/numberOfCategories)-6) 75 5
                                                                                         |> filled (rgb 150 200 250) 
                                                                                  , roundedRect ((175/numberOfCategories)-6) 75 5
                                                                                         |> outlined (solid 0.5) black     
                                                                                  , text x.name |> filled black |> scale (0.55-1.3/numberOfCategories) |> move (-12,25)
                                                                                  , createLatestExpense x.expenseList |> scale (1.2-1.5/numberOfCategories)
                                                                                 ] |> move (position,0) |> notifyTap (ToFullList x) |> notifyTap ResetScroll 
                                                                           , createPartialCategories xs numberOfCategories (position + (175/numberOfCategories))
                                                                          ]

createLatestExpense expenseList = case expenseList of
                                  [] -> group [
                                                text "Latest Expense:" |> filled black |> scale 0.3 |> move (-12,15)
                                                , text "None" |> filled black |> scale 0.3 |> move (-12,10)
                                                , text "Amount:" |> filled black|> scale 0.3 |> move (-12,0)
                                                , text "0" |> filled black |> scale 0.3 |> move (-12,-5)
                                                , text "Date:" |> filled black |> scale 0.3 |> move (-12,-15)
                                                , text "N/A" |> filled black |> scale 0.3 |> move (-12,-20)
                                              ]
                                  x::xs -> case x of
                                              Normal name amount date -> group [
                                                                                 text "Latest Expense:" |> filled black |> scale 0.32 |> move (-12,15)
                                                                                 , text name |> filled black |> scale 0.3 |> move (-12,10)
                                                                                 , text "Amount:" |> filled black |> scale 0.32 |> move (-12,0)
                                                                                 , text ("$"++(String.fromFloat amount)) |> filled black |> scale 0.3 |> move (-12,-5)
                                                                                 , text "Date:" |> filled black |> scale 0.32 |> move (-12,-15)
                                                                                 , text ((String.fromInt date.day) ++ "/" ++ (String.fromInt date.month) ++ "/" ++ (String.fromInt date.year)) |> filled black |> scale 0.3 |> move (-12,-20)
                                                                               ]
                                              Recurrent name amount extra date-> group [
                                                                                 text "Latest Expense:" |> filled black |> scale 0.32 |> move (-12,15)
                                                                                 , text name |> filled black |> scale 0.3 |> move (-12,10)
                                                                                 , text "Amount:" |> filled black |> scale 0.32 |> move (-12,0)
                                                                                 , text ("$"++(String.fromFloat amount)) |> filled black |> scale 0.3 |> move (-12,-5)
                                                                                 , text "Next Automatic" |> filled black |> scale 0.32 |> move (-12,-15)
                                                                                 , text "Deduction:" |> filled black |> scale 0.32 |> move (-12,-20)
                                                                                 , text ((String.fromInt date.day) ++ "/" ++ (String.fromInt date.month) ++ "/" ++ (String.fromInt date.year)) |> filled black |> scale 0.3 |> move (-12,-25)
                                                                               ]
count_amount expenseList sum = case expenseList of
                                  [] -> sum
                                  x::xs -> let curramount = case x of
                                                              Normal name amount date -> amount 
                                                              Recurrent name amount date extra -> amount
                                              in count_amount xs (sum + curramount)


button sometext transition textposition fontsize = group
                                  [rect 50 15 |> filled darkRed
                                   ,
                                   text sometext |> filled white
                                                 |>scale fontsize
                                                 |>move textposition
                                  ] |>notifyTap transition   

buttonCat sometext transition textposition fontsize = group
                                  [rect 55 12 |> filled darkRed
                                   ,
                                   text sometext |> filled white
                                                 |>scale fontsize
                                                 |>move textposition
                                  ] |>notifyTap transition   

buttonRN model sometext transition textposition fontsize color1 color2 = group
                                  [rect 25 10 |> filled (case model.expenseTypeRN of
                                                           "Recurrent" -> color1                     
                                                           "Normal" -> color2
                                                           otherwise   -> darkRed)
                                   
                                   ,text sometext |> filled white
                                                 |>scale fontsize
                                                 |>move textposition
                                  ] |>notifyTap transition   


inputBox sometext transition textposition fontsize = group
                                  [roundedRect 55 11 5|> outlined (solid 2) green
                                   ,roundedRect 55 11 5|> filled white,
                                   text sometext |> filled black
                                                 |>scale fontsize
                                                 |>move textposition
                                  ] |>notifyTap transition   

smallInputBox sometext transition textposition fontsize = group
                                  [roundedRect 12 11 5|> outlined (solid 2) green
                                   ,roundedRect 12 11 5|> filled white,
                                   text sometext |> filled black
                                                 |>scale fontsize
                                                 |>move textposition
                                  ] |>notifyTap transition   

mediumInputBox sometext transition textposition fontsize = group
                                  [roundedRect 22 11 5|> outlined (solid 2) green
                                   ,roundedRect 22 11 5|> filled white,
                                   text sometext |> filled black
                                                 |>scale fontsize
                                                 |>move textposition
                                  ] |>notifyTap transition   

type Msg = Tick Float GetKeyState
         | ToCategories
         | ToRE
         | ToHelp
         | ToSettings
         | ToMain
         | ToFullList Category
         | ResetScroll
         | ScrollDown
         | ScrollUp
         | Drag (Float, Float)
         | SwitchMousePressState (Float, Float)
         | Change String
         | ToExpenseType
         | ToExpenseName
         | ToExpenseAmount
         | AddExpense
         | ClearInput
         | ToNday
         | ToNmonth
         | ToNyear
         | ToRday
         | ToRmonth
         | ToRyear
         | ToRExpense
         | ToNExpense
         | ToCurrCategory Category
         | ToChooseCat
         | NextDay
         | AcceptCharge Expenses
         

type State = MainMenu 
           | Categories 
           | RExpenses 
           | Help 
           | Settings 
           | FullCategory Category
           | ExpenseType
           | ExpenseName
           | ExpenseAmount
           | Nday
           | Nmonth
           | Nyear
           | Rday
           | Rmonth
           | Ryear
           | ChooseCat


type MousePressStates = Released | MouseDown (Float, Float)

update msg model =
    case msg of
        
        Tick t (keys, _, _) -> case model.state of 
                ExpenseType ->
                              { model
                                      | expenseType = model.expenseType 
                                         ++ ( List.concatMap (didType keys) allowedKeys
                                             |> String.concat
                                              )
                              }
                ExpenseName -> { model
                                      | expenseName = model.expenseName 
                                         ++ ( List.concatMap (didType keys) allowedKeys
                                             |> String.concat
                                              )
                              }
                ExpenseAmount -> { model
                                      | expenseAmount = model.expenseAmount 
                                         ++ ( List.concatMap (didType keys) allowedKeys
                                             |> String.concat
                                              )
                              }
                Nday -> { model
                                      | nDay = model.nDay
                                         ++ ( List.concatMap (didType keys) allowedKeys
                                             |> String.concat
                                              )
                              }
                Nmonth -> { model
                                      | nMonth = model.nMonth
                                         ++ ( List.concatMap (didType keys) allowedKeys
                                             |> String.concat
                                              )
                              }
                Nyear -> { model
                                      | nYear = model.nYear
                                         ++ ( List.concatMap (didType keys) allowedKeys
                                             |> String.concat
                                              )
                              }
                Rday -> { model
                                      | rDay = model.rDay 
                                         ++ ( List.concatMap (didType keys) allowedKeys
                                             |> String.concat
                                              )
                              }
                Rmonth -> { model
                                      | rMonth = model.rMonth 
                                         ++ ( List.concatMap (didType keys) allowedKeys
                                             |> String.concat
                                              )
                              }
                Ryear -> { model
                                      | rYear = model.rYear
                                         ++ ( List.concatMap (didType keys) allowedKeys
                                             |> String.concat
                                              )
                              }
                otherwise ->
                    model
        ClearInput -> case model.state of
                ExpenseType -> { model| expenseType = ""}
                ExpenseName -> { model| expenseName = ""}
                ExpenseAmount -> { model| expenseAmount = ""}
                Nday -> { model| nDay = ""}
                Nmonth -> { model| nMonth = ""}
                Nyear -> { model| nYear = ""}
                Rday -> { model| rDay = ""}
                Rmonth -> { model| rMonth = ""}
                Ryear -> { model| rYear = ""}
                otherwise ->
                    model
        ToRExpense -> case model.state of 
                MainMenu ->
                    { model | expenseTypeRN = "Recurrent" }
                otherwise ->
                    model
        ToNExpense -> case model.state of 
                MainMenu ->
                    { model | expenseTypeRN = "Normal" }
                otherwise ->
                    model
        ToCurrCategory cat -> case model.state of 
                MainMenu ->
                    { model | currentCategory = cat, currentExpenseName = "", catName = cat.name }
                otherwise ->
                    model
        AddExpense -> case model.state of
                MainMenu  ->
                    { model | 
                    expenseType = "", 
                    expenseName = "", 
                    expenseAmount = "", 
                    nDay = "" , 
                    nMonth = "", 
                    nYear = "", 
                    rDay = "", 
                    rMonth = "", 
                    rYear = "", 
                    expenseTypeRN = "",
                    catName = "",
                    categories = (List.map (\x -> if ((x.name) == (model.currentCategory.name)) then {x | expenseList = x.expenseList ++ [createExpenseFromInput model]} else x) model.categories)  
                    } 
                otherwise ->
                    model
        ToExpenseType -> case model.state of
                MainMenu  ->
                    { model | state = ExpenseType  } 
                otherwise ->
                    model
        ToExpenseAmount -> case model.state of
                MainMenu  ->
                    { model | state = ExpenseAmount  } 
                otherwise ->
                    model
        ToChooseCat -> case model.state of
                MainMenu  ->
                    { model | currentExpenseName = "choosing"   } 
                otherwise ->
                    model
        ToExpenseName -> case model.state of
                MainMenu  ->
                    { model | state = ExpenseName  } 
                otherwise ->
                    model
        ToNday -> case model.state of
                MainMenu  ->
                    { model | state = Nday  } 
                otherwise ->
                    model
        ToNmonth -> case model.state of
                MainMenu  ->
                    { model | state = Nmonth  } 
                otherwise ->
                    model
        ToNyear -> case model.state of
                MainMenu  ->
                    { model | state = Nyear  } 
                otherwise ->
                    model
        ToRday -> case model.state of
                MainMenu  ->
                    { model | state = Rday  } 
                otherwise ->
                    model
        
        ToRmonth -> case model.state of
                MainMenu  ->
                    { model | state = Rmonth  } 
                otherwise ->
                    model
        ToRyear -> case model.state of
                MainMenu  ->
                    { model | state = Ryear  } 
                otherwise ->
                    model
        ToCategories ->
            case model.state of
                MainMenu  ->
                    { model | state = Categories  }
                FullCategory category ->
                    { model | state = Categories  }
                otherwise ->
                    model
        ToRE ->
            case model.state of
                MainMenu  ->
                    { model | state = RExpenses  }
                otherwise ->
                    model
        ToHelp ->
            case model.state of
                MainMenu  ->
                    { model | state = Help  }
                otherwise ->
                    model
        ToSettings ->
            case model.state of
                MainMenu  ->
                    { model | state = Settings  }
                otherwise ->
                    model
        ToMain ->
          case model.state of
            MainMenu ->
              model
            otherwise ->
              {model | state = MainMenu}
        ToFullList category ->
          case model.state of
            Categories ->
              {model | state = FullCategory category}
            otherwise ->
              model
        ResetScroll ->
          {model | scroll = 0}
        ScrollDown -> 
          {model | scroll = model.scroll-5}
        ScrollUp -> 
          {model | scroll = model.scroll+5}
        Drag (x, y)    
                        -> case model.mouse of 
                                   MouseDown (xDelta, yDelta) 
                                       -> { model | scrollPos = (0, y+yDelta) }
                                   Released 
                                       -> model
        SwitchMousePressState (x, y)  
                        -> { model | mouse = 
                              case model.mouse of
                                   Released         ->
                                       -- store the vector between the click 
                                       -- and the centre of the shape
                                       let (xPos, yPos) = model.scrollPos 
                                       in MouseDown (xPos-x, yPos-y)
                                   MouseDown _      -> Released  
                           }
        Change newContent ->
          { model | content = newContent }
        NextDay ->
          { model | date = convertMaxDates (dateSubtraction model.date {day = -1, month = 0, year = 0})
                    ,
                    categories = (alterCategory model.categories)
                    }
        AcceptCharge exp ->
          case exp of
            Recurrent _ amount _ _ ->
              { model | pendingCharges = List.filter (\otherExp -> expenseEqualityNot exp otherExp) model.pendingCharges
                   ,
                   budget = model.budget - amount
                   }
            _ -> model


type alias Model =
    { time : Float
    , state : State
    , categories : List Category
    , scroll : Float
    , date : Date
    , mouse : MousePressStates
    , scrollPos : (Float, Float)
    , content : String
    , expenseType : String
    , expenseName: String
    , expenseAmount: String
    , nDay: String
    , nMonth: String
    , nYear: String
    , rDay: String
    , rMonth: String
    , rYear: String
    , dummyExpenseList1: List Expenses
    , expenseTypeRN: String
    , currentCategory: Category
    , currentExpense: List Expenses 
    , currentExpenseName: String
    , catName: String
    , pendingCharges : List Expenses
    , budget : Float
    }

init : Model
init = { time = 0 
       , state = MainMenu
       , categories = [healthCare,food,transportation,housing,utilities]
       , scroll = 0
       , date = {
           day = 1,
           month = 0,
           year = 0
         }
       , mouse = Released
       , scrollPos = (0,0)
       , content = ""
       , expenseType = ""
       , expenseName = ""
       , expenseAmount = ""
       , nDay = ""
       , nMonth = ""
       , nYear = ""
       , rDay = ""
       , rMonth = ""
       , rYear = ""
       , dummyExpenseList1 = []
       , expenseTypeRN = ""
       , currentCategory = food
       , currentExpense = []
       , currentExpenseName = ""
       , catName = ""
       , pendingCharges = []
       , budget = 8000
       }
 
allowedKeys = String.split "" " abcdefghijklmnopqrstuvwxyz1234567890"

alterCharges : List Category -> List Expenses
alterCharges categories = List.concat (List.map alterChargeExpenses categories)

alterChargeExpenses : Category -> List Expenses
alterChargeExpenses cat = List.filter checkForNothing (List.map grabDue cat.expenseList)

grabDue : Expenses -> Expenses
grabDue exp =
  case exp of
    Normal _ _ _ -> Recurrent "" 0 {day = 0, month = 0, year = 0} {day = 0, month = 0, year = 0}
    Recurrent _ _ _ countdown ->
      let
        nextCountDown = dateSubtraction countdown {day = 1, month = 0, year = 0}
      in
        if nextCountDown == {day = 0, month = 0, year = 0} then
          exp
        else
          Recurrent "" 0 {day = 0, month = 0, year = 0} {day = 0, month = 0, year = 0}



alterCategory : List Category -> List Category
alterCategory categories = List.map alterCategoryExpenses categories

alterCategoryExpenses : Category -> Category
alterCategoryExpenses cat = { cat | expenseList = countDownRecurrent cat.expenseList}

countDownRecurrent : List Expenses -> List Expenses
countDownRecurrent ls = List.map checkForCountDown ls

checkForCountDown : Expenses -> Expenses
checkForCountDown expense =
  case expense of
    Normal _ _ _ -> expense
    Recurrent a b auto countdown ->
      let
        nextCountDown = dateSubtraction countdown {day = 1, month = 0, year = 0}
      in
        if nextCountDown /= {day = 0, month = 0, year = 0} then
          Recurrent a b auto nextCountDown
        else
          Recurrent a b auto auto
          
checkForNothing : Expenses -> Bool
checkForNothing exp = 
  if exp == Recurrent "" 0 {day = 0, month = 0, year = 0} {day = 0, month = 0, year = 0} then
    False
  else
    True

makeDueExpense exp =
  case exp of
  Recurrent name amount next _ ->
    group [
      roundedRect 200 100 10
        |> filled (rgb 150 200 250)
      ,
      text ("The automatic expense " ++ name ++ " is due today. Automatically deducting " ++ (String.fromFloat amount) ++ " from budget.")
        |> filled black
        |> scale 0.3
        |> move (-80, 0)
      ,
      text ("The next deduction is " ++ (dateToString (convertMaxDates next)) ++ " from now.")
        |> filled black
        |> scale 0.3
        |> move (-80, -20)
      ,
      button "Ok" (AcceptCharge exp) (0,0) 0.3
        |> move (0, -40)
    ]
  
  _ -> group []

buildDueExpenseList model =
  if model.pendingCharges == [] then
    group []
  else
    group <|
      (square 500
        |> filled black
        |> makeTransparent 0.01
    ) :: (List.map makeDueExpense model.pendingCharges)

expenseEqualityNot : Expenses -> Expenses -> Bool
expenseEqualityNot testexp actexp =
  if testexp == actexp then
    False
  else
    True


buildReExpense exp =
  case exp of
    Recurrent name amount initDate countdown -> group [
      roundedRect 200 80 10
        |> filled (rgb 150 200 250)
        |> move (60, 0)
      ,
      roundedRect 200 80 10
        |> outlined (solid 0.5) black
        |> move (60, 0)
      ,
      text name
        |> filled black
        |> move (-10, 20)
      ,
      text ("Cost - $" ++ (String.fromFloat amount))
        |> filled black
        |> move (-20, 0)
        |> scale 0.8
      ,
      text ("Auto-Payment - " ++ (dateToString initDate))
        |> filled black
        |> move (-20, -20)
        |> scale 0.8
      ,
      text ("Next-Payment - " ++ (dateToString countdown))
        |> filled black
        |> move (-20, -40)
        |> scale 0.8
      ]
    _ -> group []

buildListOfRe : Model -> Shape userMsg
buildListOfRe model = group <| applyTransforms (List.concat (List.map buildReForCat model.categories)) 0

--buildReForCat : Category -> List (Shape userMsg)
--buildReForCat cat = List.map buildReExpense cat.expenseList

buildReForCat : Category -> List (Shape userMsg)
buildReForCat cat = List.filter expTest (List.map buildReExpense cat.expenseList)

applyTransforms : List (Shape userMsg) -> Int -> List (Shape userMsg)
applyTransforms ls n =
  case ls of
    x :: xs ->
      --if x /= group [] then
        [x |> scale 0.5 |> scaleX 0.9 |> move (-66, toFloat (28 - 45*n))] ++ applyTransforms xs (n + 1)
      --else
      --  applyTransforms ls n
    [] -> []

expTest exp =
  if exp /= group[] then
    True
  else
    False
 
 
-- An Expense is as follows: Name, Cost, and date of expenditure. For recurrent, the only change is that date is the time of the next automatic deduction. 
type Expenses = Normal String Float Date
                | Recurrent String Float Date Date

--type ExpenseTypeRN = Normal | Recurrent
type alias Category = {
  expenseList : List Expenses,
  name : String
  }

--newDummy model = model.dummyExpenseList1 ++ [dummyExpense]

healthCare : Category
healthCare = {
    expenseList = [Normal "medicine" 50 {day=1,month=2,year=2021},Normal "more medicine" 10 {day=30,month=2,year=2021}],
    name = "Healthcare"
  }

food : Category
food = {
    expenseList = [dummyExpense],
    name = "Food"
  }
  
transportation : Category
transportation = {
    expenseList = [dummyExpense],
    name = "Transportation"
  }
  
housing : Category
housing = {
    expenseList = [dummyExpense],
    name = "Housing"
  }

utilities : Category
utilities = {
    expenseList = [dummyExpense],
    name = "Utilities"
  }

type alias Date = {
    day : Int,
    month : Int,
    year : Int
  }


dateToString : Date -> String
dateToString date =
  let
    day = date.day
    month = date.month
    year = date.year
  in
    (getValidDate year "year")
    ++
    (getValidDate month "month")
    ++
    (getValidDate day "day")

getValidDate : Int -> String -> String
getValidDate amnt unit = 
  if amnt == 0 then
    ""
  else
    if unit /= "day" then
      (String.fromInt amnt) ++ " " ++ unit ++ (getCorrectGrammer amnt) ++ ", "
    else
      (String.fromInt amnt) ++ " " ++ unit ++ (getCorrectGrammer amnt)

getCorrectGrammer : Int -> String
getCorrectGrammer dateNum =
  if dateNum == 1 then
    ""
  else
    "s"

convertMaxDates : Date -> Date
convertMaxDates date = 
  let
    numDays = date.day + date.month*30 + date.year*12*30
    properDays = modBy 30 numDays
    properMonths = modBy 12 ((numDays - (modBy 30 numDays)) // 30)
    properYears = ((numDays - (modBy 30 numDays)) // 30) // 12
  in
    {
      day = properDays
      ,
      month = properMonths
      ,
      year = properYears
    }

-- Used when we have to subtract dates for recurrent stuff.
dateSubtraction : Date -> Date -> Date
dateSubtraction date1 date2 =
  let
    numDays1 = date1.day + date1.month*30 + date1.year*12*30
    numDays2 = date2.day + date2.month*30 + date2.year*12*30
    subDays = numDays1 - numDays2
    newDays = modBy 30 subDays
    newMonths = modBy 12 ((subDays - (modBy 30 subDays)) // 30)
    newYears = ((subDays - (modBy 30 subDays)) // 30) // 12
  in
    if subDays <= 0 then
      {day = 0, month = 0, year = 0}
    else
      {
        day = newDays
        ,
        month = newMonths
        ,
        year = newYears
      }
      
scrollBar model = group [
                  roundedRect 3 100 1 
                     |> filled red
                     |> move (-87, 0)
                     |> notifyMouseDownAt SwitchMousePressState                     
                  ,
                     case model.mouse of                       
                       MouseDown _ -> rect 185 120 |> ghost 
                                                   |> notifyMouseUpAt SwitchMousePressState 
                                                   |> notifyMouseMoveAt Drag
                                                   |> notifyLeaveAt SwitchMousePressState 
                       Released  -> group []
  ]

