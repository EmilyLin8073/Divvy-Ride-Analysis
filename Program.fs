//
// F# program to analyze Divvy daily ride data.
//
// <<Emily Lin >>
// U. of Illinois, Chicago
// CS 341, Fall 2019
// Project #04
//

#light

module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [15,22,141,17,5,1124]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), starting day of week (0 Sunday-6 Saturday)
// and trip duration (secs), 
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides


//----------Helper functions-------------------
//Get Nth element
let rec getNth n L = 
    match n, L with
    | 0, head::tail -> head
    | _, head::tail -> getNth (n - 1) tail


//Get 2 Nth elements
let rec getBikeNTime L = 
    match L with
    | [] -> []
    | [e0;e1;e2;e3;e4;e5]::tail -> [e2;e5]::getBikeNTime tail
    | head::tail -> getBikeNTime tail

let rec getStatNTime L = 
    match L with
    | [] -> []
    | [e0;e1;e2;e3;e4;e5]::tail -> [e1;e5]::getStatNTime tail
    | head::tail -> getStatNTime tail
    

//Remove duplicate element in the list
let rec removeDup L e = 
    match L with
    | [] -> false
    | head::tail -> if head = e then true else removeDup tail e


//Get the bike list
let rec bikeList L = 
    match L with
    | [] -> []
    | head::tail -> (getNth 2 head)::bikeList tail


//Get the station list
let rec stationList L = 
    match L with
    | [] -> []
    | head::tail -> (getNth 1 head)::stationList tail


//Get the day list
let rec dayList L = 
    match L with
    | [] -> []
    | head::tail -> (getNth 4 head)::dayList tail


//Delete the duplicate bikeID
let bikeID L = 
    let rec removeDupBikeID L L2 = 
        match L with 
        | [] -> L2
        | head::tail when removeDup L2 head = false -> removeDupBikeID tail (head::L2)
        | head::tail -> removeDupBikeID tail L2
    removeDupBikeID L []


//Remove duplicate StationID
let statID L = 
    let rec removeDupStatID L L2 = 
        match L with 
        | [] -> L2
        | head::tail when removeDup L2 head = false -> removeDupStatID tail (head::L2)
        | head::tail -> removeDupStatID tail L2
    removeDupStatID L []


//Print out the stars
let rec printStars n = 
    match n with 
    | 0 -> ()
    | 1 -> printf "*"
    | _ -> printf "*"
           printStars (n - 1)


//Count the number of the element in the list
let rec count x L = 
  match L with
  | [] -> 0
  | head::tail when head = x -> 1 + (count x tail)
  | head::tail -> 0 + (count x tail)
//----------------------------------------------


//Find # of rides for the user input bikeID
let rec numForBikeID L bikeID= 
    match L with 
    | [] -> 0
    | head::tail when bikeID = head -> 1 + numForBikeID tail bikeID
    | head::tail -> numForBikeID tail bikeID


//Find # of stations for the user input stationID
let rec numOfStatID stationList stationID = 
    match stationList with
    | [] -> 0
    | head::tail when stationID = head -> 1 + numOfStatID tail stationID
    | head::tail -> numOfStatID tail stationID


//Find # on Sunday
let rec numOnSunday L = 
    match L with
    | [] -> 0
    | head::tail when head = 0 -> 1 + numOnSunday tail
    | head::tail -> numOnSunday tail


//Find # on Monday
let rec numOnMonday L = 
    match L with
    | [] -> 0
    | head::tail when head = 1 -> 1 + numOnMonday tail
    | head::tail -> numOnMonday tail


//Find # on Tuesday
let rec numOnTuesday L = 
    match L with
    | [] -> 0
    | head::tail when head = 2 -> 1 + numOnTuesday tail
    | head::tail -> numOnTuesday tail


//Find # on Wednesday
let rec numOnWednesday L = 
    match L with
    | [] -> 0
    | head::tail when head = 3 -> 1 + numOnWednesday tail
    | head::tail -> numOnWednesday tail


//Find # on Thursday
let rec numOnThursday L = 
    match L with
    | [] -> 0
    | head::tail when head = 4 -> 1 + numOnThursday tail
    | head::tail -> numOnThursday tail


//Find # on Friday
let rec numOnFriday L = 
    match L with
    | [] -> 0
    | head::tail when head = 5 -> 1 + numOnFriday tail
    | head::tail -> numOnFriday tail


//Find # on Saturday
let rec numOnSaturday L = 
    match L with
    | [] -> 0
    | head::tail when head = 6 -> 1 + numOnSaturday tail
    | head::tail -> numOnSaturday tail


//Get the time list from the user input bikeID
let rec timeListFromBikeID L n = 
    match L with
    | [] -> []
    | [e1;e2]::tail when e1 = n -> e2::timeListFromBikeID tail n
    | head::tail -> timeListFromBikeID tail n


//Calculate the total time spend on user input bikeID
let rec totalTimeFromBikeID L =
    match L with 
    | [] -> 0
    | head::tail -> head + totalTimeFromBikeID tail 

//Get the time list from the user input station
let rec timeListFromStatID L n =
    match L with
    | [] -> []
    | [e1;e2]::tail when e1 = n -> e2::timeListFromStatID tail n
    | head::tail -> timeListFromStatID tail n


//Calculate the total time spend on user input stationID
let rec totalTimeToStatID L =
    match L with 
    | [] -> 0
    | head::tail -> head + totalTimeToStatID tail


//Count the number for each station
let rec countStatID L1 L2 = 
    match L2 with
    | [] -> []
    | head::tail -> [head;(count head L1)]::countStatID L1 tail


//print the top 10 stationID list
let printTopTenList L = 
    let rec printTopTenListRec L = 
        match L with
        | [] -> ()
        | [e1;e2]::tail -> printfn "# of rides to station %A: %A" e1 e2
                           printTopTenListRec tail
    printTopTenListRec L

    
[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of rides: %A" N
  printfn ""

  //Print out the # of bikes
  let getBikesList = bikeList ridedata
  let removeDupBikeID = bikeID getBikesList 
  let numBikes = List.length removeDupBikeID
  printfn "# of bikes: %A" numBikes
  printfn ""

  //Ask user to input the biekID
  printf "BikeID> "
  //Read in user input
  let userInputBikeID = System.Console.ReadLine()
  //Transfer user input into int
  let userInputBikeIDInt = userInputBikeID |> int
  //Count the number of bikes that match the user input
  let getNumOfBikeID = numForBikeID getBikesList userInputBikeIDInt
  printfn ""
  printfn "# of rides for BikeID %A: %A" (userInputBikeIDInt) (getNumOfBikeID)
  printfn ""

  //Print the time for that bikeID
  let bikeNTimeList = getBikeNTime ridedata
  let getTimeListFromBikeID = timeListFromBikeID bikeNTimeList userInputBikeIDInt
  let getTotalTimeFromBikeID = totalTimeFromBikeID getTimeListFromBikeID
  //Get the minutes from total time
  let totalTimeMinutes = getTotalTimeFromBikeID / 60
  //Get the second from total time
  let totalTimeSecond = getTotalTimeFromBikeID - (totalTimeMinutes * 60)
  printfn "Total time spent riding BikeID %A: %A minutes %A seconds" userInputBikeIDInt totalTimeMinutes totalTimeSecond
  printfn ""

  //Print average time for that biekID
  let getNumOfBikeIDFloat = getNumOfBikeID |> float
  let getTotalTimeFromBikeIDFloat = getTotalTimeFromBikeID |> float
  let avgTimeOnBikeID = getTotalTimeFromBikeIDFloat / getNumOfBikeIDFloat
  printfn "Average time spent riding BikeID %A: %.2f seconds" userInputBikeIDInt avgTimeOnBikeID
  printfn ""

  //Ask user for stationID
  printf "StationID> "
  let userInputStatID = System.Console.ReadLine()
  //Transfer user input into int
  let userInputStatIDInt = userInputStatID |> int
  //Get the station list
  let getStationList = stationList ridedata
  //Count the number of station that match the user input
  let getNumOfStatID = numOfStatID getStationList userInputStatIDInt
  printfn ""
  printfn "# of rides to StationID %A: %A" (userInputStatIDInt) (getNumOfStatID)
  printfn ""

  //Print average time for that station
  let statNTimeList = getStatNTime ridedata
  let getTimeListFromStatID = timeListFromStatID statNTimeList userInputStatIDInt
  let getTotalTimeToStatID = totalTimeToStatID getTimeListFromStatID
  let getTotalTimeToStatIDFloat = getTotalTimeToStatID |> float
  let getNumOfStatIDFloat = getNumOfStatID |> float
  let avgTimeToStat = getTotalTimeToStatIDFloat / getNumOfStatIDFloat
  printfn "Average time spent on trips leading to StationID %A: %.2f seconds" userInputStatIDInt avgTimeToStat
  printfn ""

  //Get the day list
  let getDayList = dayList ridedata

  //Print out # of trips on Sunday
  let getNumOnSunday = numOnSunday getDayList
  printfn "Number of Trips on Sunday: %A" getNumOnSunday

  //Print out # of trips on Monday
  let getNumOnMonday = numOnMonday getDayList
  printfn "Number of Trips on Monday: %A" getNumOnMonday

  //Print out # of trips on Tuesday
  let getNumOnTuesday = numOnTuesday getDayList
  printfn "Number of Trips on Tuesday: %A" getNumOnTuesday

  //Print out # of trips on Wednesday
  let getNumOnWednesday = numOnWednesday getDayList
  printfn "Number of Trips on Wednesday: %A" getNumOnWednesday

  //Print out # of trips on Thursday
  let getNumOnThursday = numOnThursday getDayList
  printfn "Number of Trips on Thursday: %A" getNumOnThursday

  //Print out # of trips on Friday
  let getNumOnFriday = numOnFriday getDayList
  printfn "Number of Trips on Friday: %A" getNumOnFriday

  //Print out # of trips on Saturday
  let getNumOnSaturday = numOnSaturday getDayList
  printfn "Number of Trips on Saturday: %A" getNumOnSaturday
  printfn ""

  //Print out the day stars
  //Get # of stars for Sunday
  let getSunStars = getNumOnSunday / 10
  printf "0: "
  //print # of Sunday stars
  let printSunStars = printStars getSunStars 
  printf " %A" getNumOnSunday
  printfn ""

  //Get # of stars for Monday
  let getMonStars = getNumOnMonday / 10
  printf "1: "
  //print # of Sunday stars
  let printMonStars = printStars getMonStars 
  printf " %A" getNumOnMonday
  printfn ""

  //Get # of stars for Tuesday
  let getTueStars = getNumOnTuesday / 10
  printf "2: "
  //print # of Sunday stars
  let printTueStars = printStars getTueStars 
  printf " %A" getNumOnTuesday
  printfn ""

  //Get # of stars for Wednesday
  let getWedStars = getNumOnWednesday / 10
  printf "3: "
  //print # of Sunday stars
  let printWedStars = printStars getWedStars 
  printf " %A" getNumOnWednesday
  printfn ""

  //Get # of stars for Thursday
  let getThuStars = getNumOnThursday / 10
  printf "4: "
  //print # of Sunday stars
  let printThuStars = printStars getThuStars 
  printf " %A" getNumOnThursday
  printfn ""

  //Get # of stars for Friday
  let getFriStars = getNumOnFriday / 10
  printf "5: "
  //print # of Sunday stars
  let printFriStars = printStars getFriStars 
  printf " %A" getNumOnFriday
  printfn ""

  //Get # of stars for Saturday
  let getSatStars = getNumOnSaturday / 10
  printf "6: "
  //print # of Sunday stars
  let printSatStars = printStars getSatStars 
  printf " %A" getNumOnSaturday
  printfn ""

  //Print out the top 10 stations
  let sortedGetStationList = List.sort getStationList
  let removeDupStatIDList = statID sortedGetStationList
  let getCountStatID = countStatID getStationList removeDupStatIDList
  let sortedStatID = getCountStatID |> List.sortBy(fun [e1;e2] -> e1)
  let sortedStatIDCount = sortedStatID |> List.sortBy(fun [e1;e2] -> -e2 - 1)
  let getTopTenStatIDList = List.take 10 sortedStatIDCount
  printfn "" 
  let getPrintTopTenListRec = printTopTenList getTopTenStatIDList
  printfn ""

  
  0 
