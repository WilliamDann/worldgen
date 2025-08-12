// base for actors in the world
trait Actor:
  def id: String
  def ipPerTurn(state: WorldState): Int
  def generateIntents(state: WorldState): List[Intent]
