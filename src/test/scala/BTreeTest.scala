import org.scalatest.flatspec.AnyFlatSpec
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers

class BTreeTest extends AnyFlatSpec with Checkers {
  
    behavior of "BTree" 
    
    it should "be instantiable as empty" in {
        val emp: BTree[Int] = BTree.empty(m = 5)
        assert(emp.keys.isEmpty)
        assert(emp.children.isEmpty)

        check(Prop.forAll((i : Int) => !emp.contains(i)))
    }

    it should "be able to split a full leaf" in {
        
        var rt: BTree[Int] = new BTree(Seq(6), Seq(new BTree(Seq(0, 0, 1, 2), Nil), new BTree(Seq(9, 11), Nil)))
        
        rt.splitChild(0)

        assert(rt.children.length == 3)
        assert(rt.toString == "((0,0),1,(2),6,(9,11))")

        rt = new BTree(Seq(0), Seq(new BTree(Seq(-1, -1), Nil), new BTree(Seq(3, 4, 7, 8), Nil)))

        rt.splitChild(1)

        assert(rt.children.length == 3)
        assert(rt.toString == "((-1,-1),0,(3,4),7,(8))")

    }

    it should "be able to split a full node with singleton parent" in {

        var rt: BTree[Int] = new BTree(
            Seq(20), Seq(
                new BTree(Seq(-5, 3, 10, 18), Seq(
                    new BTree(Seq(-6, -6), Nil),
                    new BTree(Seq(0, 2), Nil),
                    new BTree(Seq(7, 10), Nil),
                    new BTree(Seq(12, 15), Nil),
                    new BTree(Seq(19), Nil)
                )),
                new BTree(Seq(31, 32), Nil)
            )
        )
        
        rt.splitChild(0)

        val lsplit = rt.children(0)
        val rsplit = rt.children(1)

        assert(rt.keys == Seq(10, 20))
        assert(lsplit.keys == Seq(-5, 3))
        assert(rsplit.keys == Seq(18))

    }

    it should "be able to split a full node with full parent" in {

        var rt: BTree[Int] = new BTree(
            Seq(20, 40, 50, 60), Seq(
                new BTree(Seq(-5, 3, 10, 18), Seq(
                    new BTree(Seq(-9, -8, -6, -6), Nil),
                    new BTree(Seq(-2, 0, 2, 2), Nil),
                    new BTree(Seq(4, 5, 7, 10), Nil),
                    new BTree(Seq(12, 15, 15, 16), Nil),
                    new BTree(Seq(18, 19, 19, 20), Nil)
                )),
                new BTree(Seq(31, 32, 35, 36), Nil),
                new BTree(Seq(41, 45), Nil),
                new BTree(Seq(55, 59), Nil),
                new BTree(Seq(65, 70, 71), Nil)
            )
        )
        
        rt.splitChild(0)

        val lsplit = rt.children(0)
        val rsplit = rt.children(1)

        assert(rt.keys == Seq(50))
        assert(lsplit.keys == Seq(20, 40))
        assert(rsplit.keys == Seq(60))

        print(rt)

    }

    // it should "be able to insert into a non-full leaf" in {

    //     var rt: BTree[Int] = new BTree(Seq(1, 7, 8), Nil)

    //     rt.insertNotFull(3)

    //     assert(rt.keys.length == 4)
    //     assert(rt.toString == "(1,3,7,8)")

    // }

    // it should "be able to insert into a non-full node" in {

    //     var rt: BTree[Int] = new BTree(Seq(9), Seq(
    //         new BTree(Seq(1, 1, 2, 3), Nil), 
    //         new BTree(Seq(17, 20, 21, 23), Nil)
    //     ))
    //     rt.insertNotFull(12)

    //     print(rt)
    //     assert(false)

    // }

}
