// phases of world simulation
sealed trait SimPhase
case object Primordial extends SimPhase     // world gen phase
case object Historical extends SimPhase     // inital pop gen phase
case object Political  extends SimPhase     // actor-god interation phase

object Simulation:
  def runPhase(world: WorldState, phase: SimPhase, turns: Int): WorldState =
    phase match
      case Primordial => (1 to turns).foldLeft(world){ (w, _) => runPrimordialTurn(w) }
      case Historical => (1 to turns).foldLeft(world){ (w, _) => runHistoricalTurn(w) }
      case Political  => (1 to turns).foldLeft(world){ (w, _) => runPoliticalTurn(w)  }

  // world generation turn
  def runPrimordialTurn(state: WorldState): WorldState =
    val allIntents = state.gods.flatMap(_.generateIntents(state))
    val grouped = allIntents.groupBy {
      case intent: ModifyTerrain  => intent.godId
      case intent: DestroyTerrain => intent.godId
    }
    
    val resolvedState = grouped.values.flatten.foldLeft(state)((s, intent) => intent.applyTo(s))

    resolvedState.copy(
      turn = state.turn + 1
    )

  // run historical turn, generates inital positions for pops
  def runHistoricalTurn(state: WorldState): WorldState =
    val rng = new scala.util.Random()
    val width = state.map.width
    val height = state.map.height

    // find tiles in the world that are preferred by a given race
    val newPops = allRaces.flatMap { race => 
      val prefs = for {
        x <- 0 until width
        y <- 0 until height
        tile = state.map.tiles(y)(x)
        if terrainPrefs(race).contains(tile.terrain)
      } yield (x, y)

      // populate a random tile with race
      if prefs.isEmpty then
        None
      else
        val (x, y) = prefs(rng.nextInt(prefs.length))
        Some(SeedPopulation(x.toString + "," + y.toString, race, 1000, x, y, Map()))
    }

    // update state
    state.copy(pops = newPops.toVector ++ state.pops)

  // run political turn, simulation interactions between players in the world
  def runPoliticalTurn(state: WorldState): WorldState =
    state
