package intervalidus

/**
  * A discrete domain used to define intervals.
  * @tparam Self
  *   F-bounded self type.
  */
trait DiscreteDomainLike[+Self <: DiscreteDomainLike[Self]]:
  this: Self =>

  /**
    * Alternative to toString for something that looks more like code
    */
  def toCodeLikeString: String
