package intervalidus

/**
  * Template for a type class with operations on diff actions -- create/update/delete actions (like CQRS mutation
  * commands). Used when extrapolating or applying event source-style information.
  * @note
  *   intervalidus does not have event-sourced data structures, and history of mutations are not maintained.
  *
  * @tparam A
  *   diff action type
  */
trait DiffActionLike[A]:
  extension (action: A)
    /**
      * Alternative to toString for something that looks more like code
      */
    def toCodeLikeString: String
