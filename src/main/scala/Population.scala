trait Population extends Actor:
  def id: String
  def tileX: Int
  def tileY: Int
  def ipPerTurn(state: WorldState): Int
  def generateIntents(state: WorldState): List[Intent]


sealed trait Race
case object Human extends Race
case object Elf   extends Race
case object Orc   extends Race
case object Dwarf extends Race
  
val allRaces      = List(Human, Elf, Orc, Dwarf)
val terrainPrefs  = Map(
  Human -> List(Plains, Forest),
  Elf   -> List(Forest),
  Orc   -> List(Desert),
  Dwarf -> List(Mountain)
  )
case class SeedPopulation(
  idSuffix: String,
  race    : Race,
  size    : Int,
  tileX   : Int,
  tileY   : Int,
  loyalty : Map[String, Double] // god_id -> it's influince
  // TODO ^ there's a better way! 
  ) extends Population:
  val id                                  : String = "pop_" + idSuffix
  def ipPerTurn(state: WorldState)        : Int = 0
  def generateIntents(state: WorldState)  : List[Intent] = List()
