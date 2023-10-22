package btree

import scala.collection._
import scala.annotation.tailrec

/** B-Tree implementation, generic in its key type `K`.
  *
  * @param _keys
  *   Sequence of keys in the root node of the tree.
  * @param _children
  *   Sequence of subtrees between each key in the root node of the tree.
  * @param ord
  *   Ordering of the keys which organizes the tree.
  * @param m
  *   Maximum branching factor of the tree. Defaults to 5.
  */
class BTree[K](var keys: Seq[K], var children: Seq[BTree[K]])(implicit
    val m: Int = 5,
    val ord: Ordering[K]
) {

  import ord._;

  requireBTreeInvariants

  def isLeaf: Boolean = children.isEmpty
  def isEmpty: Boolean = keys.isEmpty
  def isFull: Boolean = keys.length == m - 1

  /** Size of this B-Tree, i.e. the number of keys in the tree.
    *
    * @return
    *   the number of keys in this tree
    */
  def size: Int = {
    return keys.length + children.map(_.size).fold(0)((x: Int, y: Int) => x + y)
  }

  /** Depth of this B-Tree, with 1 as the depth of the root.
    *
    * @return
    *   the depth of the deepest leaf in this B-Tree.
    */
  def depth: Int = {
    if (isLeaf) {
      return 1
    }
    return 1 + children.map(_.depth).max
  }

  /** Traverse the B-Tree in-order.
    *
    * @return
    *   Elements of this B-Tree, in-order.
    */
  def elementsInOrder: Seq[K] = {
    if (isLeaf) {
      return keys
    }
    return children.map(_.elementsInOrder).zip(keys).flatMap(x => x._1 :+ x._2) ++ children.last.elementsInOrder
  }

  /** Asserts that the B-Tree invariants hold on this B-Tree instance.
    */
  private def requireBTreeInvariants = {
    try {
      require(m > 0, "B-Tree order, `m`, must be positive")
      require(children.length <= m, "B-Tree must have `|C| <= m`")
      require(
        (keys.length == children.length - 1) || isLeaf,
        "B-Tree node must have exactly `|C| - 1` keys"
      )
      require(
        (keys.sorted == keys),
        "B-Tree keys must be sorted"
      )
      require(children.filter(_.isEmpty).isEmpty, "B-Tree child nodes must have at least 1 key")
      require(
        children.map(_.keys(0)).sorted == children.map(_.keys(0)),
        "B-Tree child nodes must be sorted by head key"
      )
      require(elementsInOrder == elementsInOrder.sorted, "B-Tree elements must be sorted, when traversed in-order")
    } catch {
      case e: IllegalArgumentException => throw new IllegalArgumentException(e.getMessage ++ " in:\n" ++ toString)
    }
  }

  /** Represent this tree as a `String`.
    *
    * @return
    *   `String` representation of the tree
    */
  override def toString: String =
    "(" ++ (children
      .grouped(1)
      .zipAll(keys.grouped(1), Nil, Nil)
      .flatMap(x => x._1 ++ x._2)
      .mkString(",")) ++ ")"

  /** Inserts a given key from the root of the tree, in-place.
    *
    * @param k
    *   Key to insert into the tree
    */
  def insert(k: K): Unit = {

    if (isFull) {

      if (isLeaf) {
        splitRoot
      }

      val insertionIdx: Int = keys.takeWhile(_ <= k).length
      children(insertionIdx).insert(k)

    } else {
      insertNotFull(k)

    }
    requireBTreeInvariants
  }

  /** Inserts a given key from the root of the tree (assuming it is not full), in-place.
    *
    * @param k
    *   Key to insert into the tree
    */
  private def insertNotFull(k: K): Unit = {
    var leftKeys: Seq[K] = keys.takeWhile(_ <= k)
    if (isLeaf) {
      val rightKeys: Seq[K] = keys.dropWhile(_ <= k)
      val newKeys: Seq[K] = (leftKeys :+ k) ++ rightKeys
      keys = newKeys
    } else {
      if (children(leftKeys.length).keys.length == m - 1) {
        splitChild(leftKeys.length)
        if (keys(leftKeys.length) < k) {
          leftKeys = leftKeys ++ Seq(k)
        }
      }
      children(leftKeys.length).insert(k)
    }
  }

  /** Utility method for splitting a full root node, in-place.
    */
  private def splitRoot: Unit = {

    require(isFull)

    var newRoot: BTree[K] = BTree.empty(m)
    newRoot.children = Seq(new BTree(keys, children))

    newRoot.splitChild(0)

    keys = newRoot.keys
    children = newRoot.children

    requireBTreeInvariants

  }

  /** Utility method for splitting an over-full child node of the root by index, in-place.
    *
    * @param childIdx
    *   the index of the child node to split
    */
  private def splitChild(childIdx: Int) = {

    if (isFull) {

      splitRoot

    } else {

      var child: BTree[K] = children(childIdx)

      require(child.isFull)

      var newChild: BTree[K] = BTree.empty(m)
      val newKey: K = child.keys(m / 2)
      val newKeys: Seq[K] = keys.slice(0, childIdx) ++ Seq(newKey) ++ keys.slice(childIdx, m - 1)
      val newLKeys: Seq[K] = child.keys.slice(0, m / 2)
      val newRKeys: Seq[K] = child.keys.slice(m / 2 + 1, m)

      child.keys = newLKeys
      newChild.keys = newRKeys

      if (!child.isLeaf) {

        val newLChildren: Seq[BTree[K]] = child.children.slice(0, (m / 2.0).ceil.toInt)
        val newRChildren: Seq[BTree[K]] = child.children.slice((m / 2.0).ceil.toInt, m)

        child.children = newLChildren
        newChild.children = newRChildren

      }

      val newChildren: Seq[BTree[K]] = (
        children.slice(0, childIdx) ++
          Seq(child, newChild) ++
          children.slice(childIdx + 1, m)
      )

      keys = newKeys
      children = newChildren

    }

    requireBTreeInvariants
  }

  /** Query the tree to search for the given key.
    *
    * @param k
    *   search key
    * @return
    *   true if the key is found, false otherwise
    */
  def contains(k: K): Boolean = {
    if (keys.isEmpty) return false
    if (children.isEmpty) return keys.contains(k)
    if (k < keys(0)) {
      return children(0).contains(k)
    }
    for (idx <- 0 until keys.length) {
      if (k == keys(idx)) return true
      if (
        idx < keys.length - 1 &&
        k > keys(idx) &&
        k < keys(idx + 1)
      ) return children(idx + 1).contains(k)
    }
    if (k > keys.last) {
      return children.last.contains(k)
    }
    return false
  }

}

/** Companion object, useful for instantiating empty B-Trees.
  */
object BTree {
  def empty[K](m: Int = 5)(implicit ord: Ordering[K]): BTree[K] = new BTree[K](Nil, Nil)(m, ord)
}
