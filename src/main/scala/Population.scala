// represents a large group of people in the game world
//  this is seperate from individual actors in the world
//  these simulate groups
trait Population:
  def tileX   : Int
  def tileY   : Int
  def groups  : Vector[PopulationGroup]

// represents information about a group of people in the world
case class PopulationGroup(
  race    : Race,
  size    : Int,
  loyalty : Map[String, Double] // percentage (0.0 - 1.0) of size that has allegance to a god
)

// TODO populate
enum Race:
  case Human, Goblin, Orc
