object Simulation:
  def simulateTurn(state: WorldState): WorldState =
    val allIntents = state.gods.flatMap(_.generateIntents(state))
    val grouped = allIntents.groupBy {
      case intent: ModifyTerrain  => intent.godId
      case intent: DestroyTerrain => intent.godId
    }
    
    val resolvedState = grouped.values.flatten.foldLeft(state)((s, intent) => intent.applyTo(s))

    resolvedState.copy(
      turn = state.turn + 1
    )
