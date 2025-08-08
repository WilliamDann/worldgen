import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

// world state info
val gods  = GodRegistry.defaultGods
var world = WorldState.initial(gods)

// run the generation state with primordial gods
def runGenerationStage(turns: Int): Unit = 
  for (i <- 0 to turns) {
    print("sim step: ")
    println(world.turn)
    println(world.map.toAsciiString)
    println("\u001b[2J\u001b[H")
    world = Simulation.simulateTurn(world) 
  }
  println("sim complete")
  println(world.map.toAsciiString)

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
      case "his" => println("Not Implemented")
      case "pol" => println("Not Implemented")
      case "man" => println("Not Implemented")
      case "q"   => quit = true
      case _     => println("Unknon command")
    }
  }

