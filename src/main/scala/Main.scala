import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

// world state info
val gods  = GodRegistry.defaultGods
var world = WorldState.initial(gods, 50, 50)

// run the generation state with primordial gods
def runGenerationStage(turns: Int): Unit =
  if (world.map.width + world.map.height > 100) {
      world = Simulation.runPhase(world, Primordial, turns)  
  } else {
    for (i <- 0 to turns) {
      world = Simulation.runPhase(world, Primordial, 1)  
      println(world.map.toAsciiString)
      print("sim step: ")
      println(world.turn)
    }
  }

  print("\u001b[2J\u001b[H")
  println(world.map.toAsciiString)
  println("sim complete")

// seed the world with inital data
def runHistoricalStage(turns: Int): Unit =
  for (i <- 0 to turns) {
    world = Simulation.runPhase(world, Historical, 1)
  }

  print("\u001b[2J\u001b[H")
  println(world.toAsciiString)
  // world.pops.map(println)

// run a simulation of gods, actors, and population interacting in world
def runPoliticalState(turns: Int): Unit =
  println()

// sim menu
@main def godGameSim(): Unit =
  print("\u001b[2J\u001b[H")
  var quit = false
  while (!quit) {
    println("select generation mode:")
    println(" gen - generate world using primordial rules")
    println(" his - generate world using historical rules")
    println(" pol - generate world using political rules")
    println(" man - manually edit world info")
    println(" q   - quit")
    println()
    print(">")
    val inp = readLine()
    inp match {
      case "gen" => 
        print("number of turns: ")
        runGenerationStage(readInt())

      case "his" =>
        print("unmber of turns: ")
        runHistoricalStage(readInt())

      case "pol" => println("Not Implemented")
      case "man" => println("Not Implemented")
      case "q"   => quit = true
      case _     => println("Unknon command")
    }
  }

