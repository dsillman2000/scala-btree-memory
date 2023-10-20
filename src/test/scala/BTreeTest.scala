import org.scalatest.flatspec.AnyFlatSpec
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers

class BTreeTest extends AnyFlatSpec with Checkers {
  
    "BTree" should "be instantiable as empty" in {
        val emp: BTree[Int] = BTree.empty
        assert(emp.keys.isEmpty)
        assert(emp.children.isEmpty)

        check(Prop.forAll((i : Int) => !emp.contains(i)))
    }

    "BTree" should "be able to split a full leaf" in {
        
        var rt: BTree[Int] = new BTree(Seq(6), Seq(new BTree(Seq(0, 0, 1, 2), Nil), new BTree(Seq(9, 11), Nil)))
        
        rt.splitChild(0)

        println(rt)

        assert(false)
        assert(rt.children.length == 3)

    }

}
