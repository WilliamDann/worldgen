// create a terrain tile
case class ModifyTerrain(
  godId   : String,
  x       : Int,
  y       : Int,
  terrain : Terrain,
  cost    : Int
) extends Intent {
  def createdById: String = godId
  def applyTo(state: WorldState): WorldState = {
    val updatedMap = state.map.updateTile(x, y, _.copy(terrain = terrain))
    state.copy(map = updatedMap)
  }
}

// destroy a terrain tile
case class DestroyTerrain(
  godId   : String,
  x       : Int,
  y       : Int,
  cost    : Int
) extends Intent {
  def createdById: String = godId
  def applyTo(state: WorldState): WorldState = {
    val updatedMap = state.map.updateTile(x, y, _.copy(terrain = Void))
    state.copy(map = updatedMap)
  }
}
