package week7

/**
 *  Problem : given glasses with different capacities and filled with different amount of water
 *            find out how to pour the water in glasses in order to get a specified amount in one glass 
 */
class Pouring (capacity : Vector[Int]){
	type State = Vector[Int]
	val initialState = capacity map (x=>0) //initial state : all glasses are empty
	
	//moves. glass are the index in the vector, referring which glass the action is against
	trait Move{
	  def change(state:State):State
	}
	case class Empty(glass:Int) extends Move {
	  def change(state:State) = state updated (glass,0)
	}
	case class Fill(glass:Int) extends Move {
	  def change(state:State) = state updated (glass,capacity(glass))
	}
	case class Pour(from:Int,to:Int) extends Move {
	  def change(state:State) = {
	    val amount = state(from) min (capacity(to) - state(to))
	    state updated (from,state(from)-amount) updated (to,state(to)+amount)
	  }
	}
	
	val glasses = 0 until capacity.length //all glasses
	
	val moves = //all possible moves
	  (for (g <- glasses) yield Empty(g)) /*empty all glasses*/ ++
	  (for (g <- glasses) yield Fill(g))  /*fill  all glasses*/ ++
	  (for (from <- glasses;to <-glasses; if (from!=to)) yield(Pour(from,to))) /*all possible moves of pouring from one glass to another*/
	
	//A chain of moves   
//	class Path(history:List[Move]) {//the head is the last move
//	  /*
//	  //Another solution 
//	  def endState : State = trackState(history)
//	  private def trackState(xs:List[Move]):State = xs match{
//	    case Nil => initialState
//	    case move :: xs1 => move change trackState(xs1) //state after all moves except the last one are applied
//	    	                                            //then apply the last move      	                                                     	
//	  }
//	  */
//	  def endState : State = (history foldRight initialState)(_ change _) //1st parameter is move in the list,2nd parameter is accumulated state
//	  def extend(move:Move) = new Path(move::history)
//	  override def toString = (history.reverse mkString " ") + "==>" + endState
//	}
//	
//	val initialPath = new Path(Nil)
    //more efficient
	class Path(history:List[Move],val endState:State){
	  def extend(move:Move) = new Path(move::history, move change endState)
	  override def toString = (history.reverse mkString " ") + "==>" + endState	  
	}
	val initialPath = new Path(Nil,initialState)  
	  
	//from a given  set of paths,apply all possible moves,to get the next set of paths
	def from(paths:Set[Path],explored:Set[State]):Stream[Set[Path]] = 
    	if(paths.isEmpty) Stream.empty
    	else{
    		val more = for {
    			path <- paths
    			next <- moves map path.extend
    			if !(explored contains next.endState) //avoid to try explored path at every recursion 
    		} yield next
    		paths #:: from(more,explored ++ (more map (_.endState))) //this is important - the next iteration will only get called when someone is asking for
    	}
  
	def pathSets = from(Set(initialPath),Set(initialState)) //all possible paths
	
	def solution(target:Int):Stream[Path] = 
	  for{
	    pathSet <- pathSets
	    path    <- pathSet
	    if path.endState.contains(target)
	  } yield path
}