import scala.collection._

class BTree[K](var _keys: Seq[K], var _children: Seq[BTree[K]])(implicit val ord: Ordering[K]) {

    import ord._;

    val m: Int = 5

    def keys: Seq[K] = _keys
    def children: Seq[BTree[K]] = _children
    def isLeaf: Boolean = children.isEmpty

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

    override def toString: String = 
        "(" ++ (children
                .grouped(1)
                .zipAll(keys.grouped(1), Nil, Nil)
                .flatMap(x => x._1 ++ x._2)
                .mkString(",")) ++ ")"

    def insert(k: K) = {
        val root: BTree[K] = this
    }

    private def insertNotFull(k: K) = {
        if (isLeaf) {
        }
    }

    def splitChild(childIdx: Int) = {

        var child: BTree[K] = children(childIdx)

        require(child.keys.length == (m - 1))

        var newChild: BTree[K] = BTree.empty
        val newKey: K = child.keys(m / 2)
        val newKeys: Seq[K] = keys.slice(0, childIdx) ++ Seq(newKey) ++ keys.slice(childIdx, m - 1)
        val newLKeys: Seq[K] = child.keys.slice(0, m / 2)
        val newRKeys: Seq[K] = child.keys.slice(m / 2, m)

        child._keys = newLKeys
        newChild._keys = newRKeys

        if (!child.isLeaf) {
            val newLChildren: Seq[BTree[K]] = child.children.slice(0, (m / 2.0).ceil.toInt)
            val newRChildren: Seq[BTree[K]] = child.children.slice((m / 2.0).ceil.toInt, m)

            child._children = newLChildren
            newChild._children = newRChildren
        }

        val newChildren: Seq[BTree[K]] = children.slice(0, childIdx) ++ Seq(newChild) ++ children.slice(childIdx, m)

        _keys = newKeys
        _children = newChildren
    }

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

object BTree {
    def empty[K](implicit ord: Ordering[K]): BTree[K] = new BTree[K](Seq.empty, Seq.empty)
}

object Run extends App {

    val bt: BTree[Int] = new BTree[Int](
        Seq(1, 7), 
        Seq(
            new BTree(Seq(-4, -3, -2), Seq.empty), 
            new BTree(Seq(4, 5, 6), Seq.empty),
            new BTree(Seq(9, 10, 13, 15), Seq.empty)
        )
    )

    Seq(4, 5, 6) :+ 6

    println(bt)
    println(bt.contains(1))
    println(bt.contains(2))
    println(bt.contains(-3))
    println(bt.contains(17))
    println(bt.contains(15))

}