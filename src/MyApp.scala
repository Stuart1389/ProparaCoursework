/**
  * Created by jim on 06/11/2016.
  */

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn
import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try} // more functional exception handling

object MyApp extends App {

  // *******************************************************************************************************************
  // application logic

  // read data from file
  val mapdata = readFile("data.txt")
  // print data to check it's been read in correctly
  //println(mapdata)

  // define menu options as a Map of actions
  // for each menu item:
  // key is an Int, the value that will be read from the input
  // value is a function () => Boolean, i.e. no params and returns Boolean
  val actionMap = Map[Int, () => Boolean](1 -> handleOne, 2 -> handleTwo, 3 -> handleThree, 4 -> handleFour, 5 -> handleFive, 6 -> handleSix, 7 -> handleQuit)

  // loop to read input and invoke menu option
  // uses function readOption to show menu and read input
  // uses function menu to invoke menu action
  // will terminate if menu returns false
    var opt = 0
    do {
      opt = readOption
    } while (menu(opt))


  // *******************************************************************************************************************
  // FUNCTIONS FOR MENU

  // shows menu and reads input
  def readOption: Int = {
    println(
      """|Please select one of the following:
        |  1 - show winner for each season
        |  2 - show stats for a season
        |  3 - show total wins for each season
        |  4 - show average points for each season
        |  5 - show total number of points per season ranked by total points
        |  6 - show a drivers total points
        |  7 - quit""".stripMargin)
    checkInt(notMenu = false) // stops exception if user doesnt enter an int
  }

  // invokes selected menu option
  // finds corresponding function to invoke in action map using get
  // pattern matching used as get returns an Option
  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  // handlers for menu options
  // Each handler calls function to start chain which carries out respective op
  def handleOne(): Boolean = {
    showWinner()
    true
  }

  def handleTwo(): Boolean = {
    showSelSeason()
    true
  }

  def handleThree(): Boolean = {
    showTotalWins()
    true
  }

  def handleFour(): Boolean = {
    showAveragePoints()
    true
  }

  def handleFive(): Boolean = {
    showPointsPS()
    true
  }

  def handleSix(): Boolean = {
    showSelDriver()
    true
  }

  def handleQuit(): Boolean = {
    println("selected quit") // returns false so loop terminates
    false
  }


  // *******************************************************************************************************************
  // UTILITY FUNCTIONS

  // Converts values from string into a list of tuples for Map[Int, list[(String, Float, Int)]]
  // input is list from readfile
  def toTuple(input: List[String]): List[(String, Float, Int)] = {
    input.map { entry =>
      val parts = entry.split(": ") // splits into 2 parts, b4 and after :
      val name = parts(0) // name is b4 :
      val numbers = parts(1).split(" ") // splits numbers into score (b4 " "), and wins (after " ")
      (name, numbers(0).toFloat, numbers(1).toInt) // return tuple, driver | points | wins
    }
  }

  // reads data file - comma separated file
  def readFile(filename: String): Map[Int, List[(String, Float, Int)]] = {
    // create buffer to build up map as we read each line
    var mapBuffer: Map[Int, List[(String, Float, Int)]] = Map()
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        // for each line
        val splitline = line.split(",").map(_.trim).toList // split line at , and convert to List
        val drivers = toTuple(splitline.tail) // head is key, tail is values. Convert values into list of tuples
        // add element to map buffer
        // splitline is line from file as List, e.g. List(Bayern Munich, 24)
        // use head as key
        // tail is a list, but need just the first (only in this case) element, so use head of tail and convert to int
        mapBuffer = mapBuffer ++ Map(splitline.head.toInt -> drivers) // add key pair to map, key and list of tuples(drivers)
      }
    } catch {
      case ex: Exception => println("Sorry, an exception happened." + ex)
    }
    mapBuffer
  }

  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER
  // each of these functions accepts user input if required for an operation,
  // invokes the relevant operation function and displays the results


  // FUNCTIONS STARTING CHAIN
  // starts show winner of each season chain
  def showWinner(): Unit = {
    print("Top drivers each season:\n")
    displayKeyVals(getTopDriverSeason())
  }

  // starts show selected season chain
  def showSelSeason(): Unit = {
    println("Enter a season to see stats:")
    sortYear(mapdata).keys.foreach(println) // prints years in order to show user available years
    val selSeason = checkInt() // function gets user input
    if (selSeason != 0){ // 0 backs the user out of the loop
      displayKeyVals(getSeason(selSeason)) // if user doesn't want to back out then try to get season
    }
  }

  // Starts show total wins chain
  def showTotalWins(): Unit = {
    print("Wins per season:\n")
    displayKeyValsSingle(getTotal(3))
  }

  // starts average points chain
  def showAveragePoints(): Unit = {
    print("Average points per season:\n")
    displayKeyValsSingle(getAverage())
  }

  // starts show points per season chain
  def showPointsPS(): Unit = {
    print("Points per season:\n")
    // invert changes tuple position for the sort (e.g. ._2 or ._3)
    displayKeyValsSingle(getTotal(2), invert = true)
  }

  // starts show selected drivers points chain
  def showSelDriver(): Unit = {
    val uniqueDrivers = getUniqueDrivers(mapdata, List.empty[String]) // gets a list of drivers for the user to select
    print("Available drivers:\n")
    uniqueDrivers.foreach(println) // displays above list
    print("Please enter a drivers full or last name:\n")
    val input: String = StdIn.readLine() // gets user input as a string
    val selectedDriver = selectDriver(input) // inserts returned value into selectedDriver
    selectedDriver match {
      case (_, _, true) => println(s"Driver: ${selectedDriver._1} has ${selectedDriver._2} points!") // if the driver exists then show their points
      case _ => print("Driver not found\n") // if driver doesn't exist, then let user know
    }
  }

  // FUNCTIONS DISPLAYING RESULTS OF INPUTS AND OPERATIONS
  // checkInt is used to stop exceptions if user enters an illegal value
  @tailrec
  def checkInt(notMenu: Boolean = true): Int = {
    if (notMenu) println("Type 0 to cancel\nPlease enter a number:") // dont want to interfere with main menu
    Try(scala.io.StdIn.readInt()) match { // Try, a more functional approach to exception handling according to docs
      // cases for successful input with no exception
      case Success(0) if notMenu => // lets the user break out of the loop
        println("Operation cancelled.")
        0
      case Success(value) => value
      // case for exception below, let user know they entered an invalid input and try again
      case Failure(_) =>
        if (!notMenu) readOption // if exception happens while in menu, then go back to menu
        else {
          println("Invalid input. Please try again.")
          checkInt(notMenu) // recursive, repeat until accepted input
        }
    }
  }

  // Function displays driver stats when a map with a list of tuples is provided as values
  def displayKeyVals(func: => Map[Int, List[(String, Float, Int)]]): Unit = {
    // Get the initial map from function argument
    val operatedMap = func
    // Call sort map to sort keys by value using map from function
    val sortedMap = sortYear(operatedMap)
    // Iterate through map
    for ((k, v) <- sortedMap) {
      println(s"$k") // Prints the key on its own line (season )
      // print each drivers stats on a new line before respective season
      println(v.sortBy(-_._2).map { // sort tuples by float, aka sort drivers by their points
        case (driver, score, wins) => // tuple(String, Float, Int)
          s"Driver: $driver, Score: ${if (score == score.toInt) score.toInt else f"$score%.1f"}, Wins: $wins"
      }.mkString("\n")) // Join by newline to separate each driver on own line
    }
  }

  // Function displays key and value pairs when value is a single value, e.g. total wins ,etc
  def displayKeyValsSingle(func: => Map[Int, Float], invert: Boolean = false): Unit = {
    // sort map keys or values depending on inverted
    val sortedMap = sortYearFloat(func, invert)
    // Iterate through map
    for ((k, v) <- sortedMap) {
      // Print the value as int if no fp is needed to accurately represent val
      println(s"$k - ${if (v == v.toInt) v.toInt else f"$v%.1f"}")
    }
  }

  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user

  // Sorting values, kinda a dumb implementation
  def sortYear(value:Map[Int, List[(String, Float, Int)]]): Map[Int, List[(String, Float, Int)]] = {
    // sort map by value in descending order -
    // see http://alvinalexander.com/scala/how-to-sort-map-in-scala-key-value-sortby-sortwith
    // always sort by key
    ListMap(value.toSeq.sortWith(_._1 > _._1): _*)
  }

  def sortYearFloat(value: Map[Int, Float], invert: Boolean = false): Map[Int, Float] = {
    // sort by key or value depending on boolean
    if(invert){
      ListMap(value.toSeq.sortWith(_._2 > _._2): _*)
    } else {
      ListMap(value.toSeq.sortWith(_._1 > _._1): _*)
    }
  }


  // Function to find the best driver in each season
  def getTopDriverSeason(): Map[Int, List[(String, Float, Int)]] = {
    mapdata.map { case (k, v) => // for each key value pair
      // go through list of tuples, check if current driver has more points than accumulator
      val topDriver = v.foldLeft(v.head) { (currTop, currDri) => // accumulator starts with first driver
        if (currDri._2 > currTop._2) currDri else currTop // if so then they're the top bloke
      }
      k -> List(topDriver) // put back into a list to use displaykeyvals function
    }
  }

  // Function to find the total of something
  def getTotal(tupPos: Int): Map[Int, Float] = {
    mapdata.map { case (k, v) => // for each key value pair
      val total = v.foldLeft(0f) { (curTotal, elem) => // go through each tuple and set accumulator to float 0
        // case determines whether to count score (._2/tuple pos 2) or wins (._3/tuple pos 3)
        curTotal + (tupPos match {
          case 2 => elem._2   // add score to accumulator
          case 3 => elem._3   // add wins to accumulator
        })
      }
      k -> total // Map key to value total
    }
  }

  def getAverage(): Map[Int, Float] = {
    val total = getTotal(2) // get map with keys and values as total score from function
    mapdata.map { case (k, v) => // for each key, value
      val totalValue = total.getOrElse(k, 1f) // get value from map or default to float 1
      val average = totalValue / v.length // get average using total and the number of values in og map
      k -> average // return for each key
    }
  }

  def getSeason(seasonVal: Int): Map[Int, List[(String, Float, Int)]] = {
    mapdata.get(seasonVal) match {
      case Some(data) => Map(seasonVal -> data)  // If found, return a map with the season value and corresponding data
      case None =>
        println("Season not found")
        Map()// If not found, return an empty map
    }
  }

  def selectDriver(input: String): (String, Float, Boolean) = {
    // checking if user entered full or second name only
    val driverExists = input.split(" ", 2) match {
      // checking if driver exists and also storing driver formatted name in val
      case Array(_, _) =>
        mapdata.values
          .flatMap(_.filter(_._1.toLowerCase == input.toLowerCase)) // Search for exact match
          .map(_._1) // Extract matching string
          .headOption // Get first match
      case _ =>
        mapdata.values
          .flatMap(_.filter(entry =>
            entry._1.toLowerCase.split(" ", 2).lift(1).contains(input.toLowerCase) // Match second part of split
          ))
          .map(_._1) // Extract matching string
          .headOption // Get first match, or None if no match
    }

    // if driverExists is not none then:
    if(driverExists.isDefined){
      val total: Float = (for {
        (_, list) <- mapdata
        tuple <- list if tuple._1 == driverExists.get
      } yield tuple._2).sum
      //print(driverExists.get, total)
      (driverExists.get, total, true)
    }else {
      ("", 0, false)
    }
  }

  @tailrec
  def getUniqueDrivers(mapData: Map[Int, List[(String, Float, Int)]], curDrivers: List[String]): List[String] = {
    // Base case: if mapData is empty, return the current drivers
    if (mapData.isEmpty) {
      //print("cur drivers", curDrivers.sortBy(name => name.split(" ").last))
      curDrivers.sortBy(name => name.split(" ").last) // sort by second name alphabetically
    } else {
      // Get key and value from the first entry
      val (k, v) = mapData.head

      // Update curDrivers with unique drivers from the current list
      val accuCurDrivers = v.foldLeft(curDrivers) { (accumulator, tuple) =>
        val tupDriver = tuple._1
        if (!accumulator.contains(tupDriver)) tupDriver :: accumulator else accumulator
      }

      // Recur with the remaining mapData and updated curDrivers
      getUniqueDrivers(mapData.tail, accuCurDrivers)
    }
  }


}

