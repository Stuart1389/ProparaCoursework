/**
  * Created by jim on 06/11/2016.
  */

import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap
import scala.util.{Try,Success,Failure} // more functional exception handling

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
        |  5 - show total number of points for each season DEL show points asc
        |  6 - show a drivers total points
        |  7 - quit""".stripMargin)
    checkInt(notMenu = false)
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
  def handleOne(): Boolean = {
    //printMap(mapdata)
    //mnuShowPoints(currentPoints) // calls function mnuShowPoints, which invokes function currentPoints
    print("Top drivers each season:\n")
    displayKeyVals(sortYear(getTopDriver()))
    //sortYear(displayKeyVals(getTopDriver()))
    true
  }

  def handleTwo(): Boolean = {
    println("Enter a season to see stats:")
    sortYear(mapdata).keys.foreach(println)
    val selSeason = checkInt()
    if (selSeason != 0){
      displayKeyVals(sortYear(getSeason(selSeason)))
    }
    true
  }

  def handleThree(): Boolean = {
    print("Wins per season:\n")
    displayKeyValsSingle(getTotal(3))
    true
  }

  def handleFour(): Boolean = {3
    println("selected quit") // returns false so loop terminates
    false
  }

  def handleFive(): Boolean = {
    print("Points per season:\n")
    displayKeyValsSingle(getTotal(2), invert = true)
    true
  }

  def handleSix(): Boolean = {
    displayKeyVals(mapdata)
    true
  }

  def handleQuit(): Boolean = {
    println("selected quit") // returns false so loop terminates
    false
  }


  // *******************************************************************************************************************
  // UTILITY FUNCTIONS


  def checkInt(notMenu: Boolean = true): Int = {
    if (notMenu) println("Type 0 to cancel\nPlease enter a number:")

    Try(scala.io.StdIn.readInt()) match {
      case Success(0) if notMenu =>
        println("Operation canceled.")
        0
      case Success(value) => value
      case Failure(_) =>
        if (!notMenu) readOption // Reprint the menu and retry for menu input
        else {
          println("Invalid input. Please try again.")
          checkInt(notMenu)
        }
    }
  }
  // Converts values from string into a list of tuples for Map[Int, list[(etc)]]
  def toTuple(input: List[String]): List[(String, Float, Int)] = {
    input.map { entry =>
      val parts = entry.split(": ")
      val name = parts(0)
      val numbers = parts(1).split(" ")
      (name, numbers(0).toFloat, numbers(1).toInt)
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
        val drivers = toTuple(splitline.tail)
        // add element to map buffer
        // splitline is line from file as List, e.g. List(Bayern Munich, 24)
        // use head as key
        // tail is a list, but need just the first (only in this case) element, so use head of tail and convert to int
        mapBuffer = mapBuffer ++ Map(splitline.head.toInt -> drivers)
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

  def mnuShowPoints(f: () => Map[Int, List[(String, Float, Int)]]) = {
    f() foreach { case (x, y) => println(s"$x: $y") }
  }

  def mnuShowPointsForTeam(f: (String) => (String, Int)) = {
    print("Team>")
    val data = f(readLine)
    println(s"${data._1}: ${data._2}")
  }

  /*
  def displayResults(map: Map[Int, Any]): = {

  }

   */

  def displayKeyVals(value: Map[Int, List[(String, Float, Int)]]): Unit = {
    // Call sort map to sort keys by value
    val sortedMap = sortYear(value)

    // Iterate over the sorted map
    for ((k, v) <- sortedMap) {
      println(s"$k")
      // Join driver information without spaces between entries
      println(v.map {
        case (driver, score, wins) =>
          s"Driver: $driver, Score: $score, Wins: $wins"
      }.mkString("\n")) // Join by newline to separate each driver
    }
  }

  def displayKeyValsSingle(value: Map[Int, Float], invert: Boolean = false): Unit = {
    // Sort the map with or without inversion
    val sortedMap = sortYearFloat(value, invert)

    // Iterate over the sorted map
    for ((k, v) <- sortedMap) {
      // Print the value as Int if it's a whole number (no decimal)
      println(s"$k - ${if (v == v.toInt) v.toInt else v}")
    }
  }

  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user

  def sortYear(value:Map[Int, List[(String, Float, Int)]]): Map[Int, List[(String, Float, Int)]] = {
    // sort map by value in descending order -
    // see http://alvinalexander.com/scala/how-to-sort-map-in-scala-key-value-sortby-sortwith
    //print("Listmap:", ListMap(value.toSeq.sortWith(_._1 > _._1): _*))
    ListMap(value.toSeq.sortWith(_._1 > _._1): _*)
  }

  def sortYearFloat(value: Map[Int, Float], invert: Boolean = false): Map[Int, Float] = {
    if(invert){
      ListMap(value.toSeq.sortWith(_._2 > _._2): _*)
    } else {
      ListMap(value.toSeq.sortWith(_._1 > _._1): _*)
    }
  }

/*
  def currentPointsForTeam(team: String): (String, Int) = {
    val points = mapdata.get(team) match{
      case Some(p) => p
      case None => 0
    }
    (team, points)
  }
 */

  // Higher-order function to process Map keys
  def loopMapKeys[T](mapData: Map[Int, List[(String, Float, Int)]],
                     processFunc: (Int, List[(String, Float, Int)]) => (Int, T)): Map[Int, T] = {
    mapData.map { case (k, v) => processFunc(k, v) }
  }

  // Function to find the top driver
  def getTopDriver(): Map[Int, List[(String, Float, Int)]] = {
    loopMapKeys(mapdata, (k, v) => {
      val topDriver = v.foldLeft(v.head) { (currTop, currDri) =>
        if (currDri._2 > currTop._2) currDri else currTop
      }
      k -> List(topDriver)  // Wrap topDriver in a list
    })
  }

  def getTotal(tupPos: Int): Map[Int, Float] = {
    mapdata.map { case (k, v) =>
      val total = v.foldLeft(0f) { (curTotal, elem) =>
        curTotal + (tupPos match {
          case 2 => elem._2   // Add Float value from the second field of the tuple
          case 3 => elem._3   // Add Int value from the third field (no conversion needed)
        })
      }
      k -> total // Map the key to the computed total
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



  // *******************************************************************************************************************

}

// def formatTuple(map: Map[Int, Any]): Map[Int, Either[(String, Float, Int), List[(String, Float, Int)]]] = {

/*
  def loopMapKeys(): Map[Int, Any] = {

  }

  def getTopDriver(): Map[Int, (String, Float, Int)] = {
    val result = for ((k, v) <- mapdata) yield { // for each value in map
      val curWinner = v.foldLeft(v.head) { (currTop, currDri) => // fold left, accumulator start first driver
        if (currDri._2 > currTop._2) currDri else currTop // check if current driver has higher points than current max
      }
      k -> curWinner // return current winner for each key
    }
    //print(result)
    result
  }

  // Function to format the tuple as strings
  def insertIntoTuple(tuple: (String, Float, Int)): (String, String, String) = {
    ("Driver: " + tuple._1, "Score: " + tuple._2.toString, "Wins: " + tuple._3.toString)
  }

  // Function to format each tuple in the map
  def formatTuples(map: Map[Int, List[(String, Float, Int)]]): Map[Int, List[(String, String, String)]] = {
    map.map { case (key, value) =>
      key -> value.map(insertIntoTuple) // Apply insertIntoTuple to each tuple in the list
    }
  }

 */