import scala.util.Random

// a god takes actions (GodIntents) in the world aligned with their reward function
trait God extends Actor:
  def id     : String
  def name   : String
  def domain : Domain

// base class for concept gods
case class ConceptGod(
  idSuffix        : String,
  name            : String,
  domain          : Domain
) extends God:
  val id                                  : String        = "god_" + idSuffix
  def generateIntents(state: WorldState)  : List[Intent]  = List()
  def ipPerTurn(state: WorldState)        : Int           = 0

// base class for gods that are able to modify world tiles
case class PrimordialGod(
  idSuffix      : String,
  name          : String,
  domain        : Terrain,
  ipPerTurn     : Int
) extends God:
  val id : String = "god_" + idSuffix

  // generate legal 'moves' for a god
  def generateIntents(state: WorldState): List[Intent] = {
    val maxX = state.map.width
    val maxY = state.map.height

    val sourceTiles =
      for
        x <- 0 until maxX
        y <- 0 until maxY
        if state.map.tiles(x)(y).terrain == domain
      yield (x, y)

    val domainSize = sourceTiles.size
    val maxDomainSize = maxX * maxY / 10

    // Spread to orthogonal neighbors that are not already your domain
    val spreadingIntents =
      for
        (x, y) <- sourceTiles
        (nx, ny) <- neighbors(x, y, maxX, maxY)
        neighborTile = state.map.tiles(nx)(ny)
        if neighborTile.terrain != domain
      yield ModifyTerrain(id, nx, ny, domain, cost = 1)

    // Seeding: pick tiles that are Void and not adjacent to domain terrain
    val seedCandidates =
      for
        x <- 0 until maxX
        y <- 0 until maxY
        tile = state.map.tiles(x)(y)
        if tile.terrain == Void
        if neighbors(x, y, maxX, maxY).forall { case (nx, ny) =>
          state.map.tiles(nx)(ny).terrain != domain
        }
      yield ModifyTerrain(id, x, y, domain, cost = 2)

    val shouldSeed = 
      domainSize < 10 || Random.nextDouble() < 0.1

    val chosenSeeds =
      if shouldSeed then Random.shuffle(seedCandidates.toList).take(1)
      else Nil

    val chosenSpreads =
      Random.shuffle(spreadingIntents.toList).take(ipPerTurn - chosenSeeds.map(_.cost).sum)

    val allIntents = (chosenSpreads ++ chosenSeeds).distinct
    val sortedIntents = allIntents.sortBy(_.cost)

    sortedIntents.take(ipPerTurn)
  }

  def ipPerTurn(state: WorldState): Int = ipPerTurn
