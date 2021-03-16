package chess

import cats.effect.IO
import cats.{Foldable, Order, Traverse}
import cats.syntax.foldable._
import cats.syntax.parallel._

object AlphaBeta {

  trait Node[F[+_]] {
    def evaluation: Int
    def nextPossibleStates: F[Node[F]]
    def isTerminal: Boolean
  }

  /*final private case class NodeWithEvaluation[Number : Numeric : Ordering, F[_] : Foldable](
    node: Node[Number, F],
    evaluation: Number,
  ) extends Ordered[NodeWithEvaluation[Number, F]] {
    private def compare(x: Number, y: Number): Int = implicitly[Ordering[Number]].compare(x, y)

    override def compare(that: NodeWithEvaluation[Number, F]): Int =
      compare(evaluation, that.evaluation)

    def max(that: NodeWithEvaluation[Number, F]): NodeWithEvaluation[Number, F] =
      if (compare(evaluation, that.evaluation) >= 0) {
        this
      } else {
        that
      }

    lazy val negated: NodeWithEvaluation[Number, F] = copy(node, implicitly[Numeric[Number]].negate(evaluation))
  }

  def alphaBeta[Number : Numeric : Ordered, F[_] : Foldable](
    node: Node[Number, F],
    depth: Int,
    alpha: NodeWithEvaluation[Number, F],
    beta: NodeWithEvaluation[Number, F],
  ): NodeWithEvaluation[Number, F] = {
    if (depth <= 0 || node.isTerminal) {
      return NodeWithEvaluation(node, node.evaluation)
    }

    node.possibleStates.foldLeft(alpha) { case (alpha, nextMove) =>
      val evaluation = alphaBeta(nextMove, depth - 1, beta.negated, alpha.negated).negated
      //println(s"${game.initiative} ${nextMove.withThreat}")

      if (evaluation >= beta) {
        return beta
      }
      alpha max evaluation
    }
  }*/

  /*implicit private def nodeOrder[F[_]]: Order[(Int, Node[F])] = (x: (Int, Node[F]), y: (Int, Node[F])) =>
    x._1 compare y._1

  private def alphaBeta[F[_] : Foldable](
    node: Node[F],
    depth: Int,
    alpha: Int,
    beta: Int,
  ): Int = {
    if (depth <= 0 || node.isTerminal) {
      return node.evaluation
    }

    node.nextPossibleStates.foldLeft(alpha) { case (alpha, nextState) =>
      val evaluation = -alphaBeta(nextState, depth - 1, -beta, -alpha)
      //println(s"${game.initiative} ${nextMove.withThreat}")

      if (evaluation >= beta) {
        return beta
      }
      evaluation max alpha
    }
  }

  def bestNextState[F[_] : Traverse](root: Node[F], depth: Int): IO[Option[Node[F]]] =
    for {
      nextStates <- root.nextPossibleStates.parTraverse { nextState =>
        IO(alphaBeta(nextState, depth * 2 - 1, -Int.MaxValue, Int.MaxValue) -> nextState)
      }
    } yield nextStates.maximumOption.map(_._2)*/
}
