package object chess {

  implicit class SetOps[A](val set: Set[A]) extends AnyVal {

    def symDiff(set2: Set[A]): Set[A] = {
      val intersected = set intersect set2
      (set ++ set2).foldLeft(Set.empty[A])((result, elem) => if (intersected contains elem) result else result + elem)
    }

    def symDiffList(set2: Set[A]): List[A] =
      (set diff set2).toList ++ (set2 diff set)
  }
}
