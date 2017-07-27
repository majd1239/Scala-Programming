/**
  * Created by Admin on 7/26/17.
  */
class Pouring( Capacity : Vector[Int] ) {

  type State =  Vector[Int]

  val initial_state = Capacity map (_ => 0)

  trait Moves
  {
    def change (state : State) : State
  }

  case class Empty(glass : Int) extends Moves
  {
    def change(state: State) = state updated (glass,0)
  }

  case class Fill(glass : Int) extends Moves
  {
    def change(state: State) = state updated(glass, Capacity(glass) )
  }

  case class Pour(From : Int, To : Int) extends Moves
  {

    def change(state: State) =
    {
      val amount = state(From) min (Capacity(To) - state(To))

      state updated(From , state(From) - amount ) updated (To, state(To) + amount )
    }
  }

  val glasses = 0 until Capacity.length

  val moves =
      ( for (glass <- glasses) yield Empty(glass))  ++
      ( for (glass <- glasses) yield Fill(glass) )  ++
      ( for (from_glass <- glasses; to_glass <-glasses; if from_glass!=to_glass) yield Pour(from_glass,to_glass))

  class Path(history: List[Moves], val endState: State)
  {
    def extend(move: Moves) = new Path( move :: history, move change endState)

    override def toString() = (history.reverse mkString " ") + "-->" + endState
  }

  val initialPath = new Path(Nil, initial_state)

  def from(paths: Set[Path], explored: Set[State]) : Stream[Set[Path]] =
    if (paths isEmpty) Stream.empty
    else
    {
      val more = for {
        path <- paths
        next_move <- moves map path.extend
        if !(explored contains next_move.endState)
      } yield next_move

      paths #:: from(more, explored ++ (more map(_.endState)) )
    }

  val PathSets = from(Set(initialPath), Set(initial_state) )

  def solution( volume: Int): Stream[Path] =
  {
    for {
      pathSet <- PathSets
      path <- pathSet
      if path.endState contains volume
    } yield path

  }

}
