import Html as Html
import Html.Attributes as Attributes
import Html.Events as Events

myShapes model =
    case model.state of
        MainMenu  ->
            [ text "MainMenu"
                  |> centered
                  |> filled black
            ]
        Categories  ->
            [ text "Categories"
                  |> centered
                  |> filled black
            ]
        RExpenses  ->
            [ text "RExpenses"
                  |> centered
                  |> filled black
              ,
              roundedRect 100 100 5
                |> filled lightBlue
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
                |> move(40, 40)
              ,
              button "Next Day" NextDay (-10, 0) 0.5
                |> move (40, 0)
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
dummyExpense : Expenses
dummyExpense = 
  let
    autoPayment = {day = 3, month = 0, year = 0}
  in
    Recurrent "Loans" 1000 autoPayment autoPayment

type Msg = Tick Float GetKeyState
         | ToCategories
         | ToRE
         | ToHelp
         | ToSettings
         | ToMain
         | Drag (Float, Float)
         | SwitchMousePressState (Float, Float)
         | Change String
         | NextDay

type State = MainMenu 
           | Categories 
           | RExpenses 
           | Help 
           | Settings 

type MousePressStates = Released | MouseDown (Float, Float)

update msg model =
    case msg of
        Tick t _ ->
            { model | time = t }
        ToCategories ->
            case model.state of
                MainMenu  ->
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

type alias Model =
    { time : Float
    , state : State
    , categories : List Category
    , date : Date
    , mouse : MousePressStates
    , scrollPos : (Float, Float)
    , content : String
    }

init : Model
init = { time = 0 
       , state = RExpenses
       , categories = [healthCare, food, transportation, housing, utilities]
       , date = {
           day = 1,
           month = 0,
           year = 0
         }
       , mouse = Released
       , scrollPos = (0,0)
       , content = ""
       }

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
          


buildReExpense exp =
  case exp of
    Recurrent name amount initDate countdown -> group [
      roundedRect 200 80 10
        |> filled blue
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
      text ("Auto-Payment - " ++ (dateToString (convertMaxDates initDate)))
        |> filled black
        |> move (-20, -20)
        |> scale 0.8
      ,
      text ("Next-Payment - " ++ (dateToString (convertMaxDates countdown)))
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

type alias Category = {
  expenseList : List Expenses,
  name : String
  }

healthCare : Category
healthCare = {
    expenseList = [dummyExpense],
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
    expenseList = [],
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
button sometext transition textposition fontsize = group[rect 50 15 |> filled darkRed,
                                   text sometext |> filled white
                                   |>scale fontsize
                                   |>move textposition]
                                   |>notifyTap transition

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
