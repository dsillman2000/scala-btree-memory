import org.scalatest.flatspec.AnyFlatSpec
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BTreeTest extends AnyFlatSpec with ScalaCheckPropertyChecks {
  
    behavior of "BTree" 
    
    it should "be instantiable as empty" in {
        val emp: BTree[Int] = BTree.empty(m = 5)
        assert(emp.keys.isEmpty)
        assert(emp.children.isEmpty)

        forAll((i : Int) => !emp.contains(i))
    }

    it should "be able to split a full leaf" in {

        /**
          * Split leaf on the left of root
          */
        forAll { (i: Int) =>

            whenever (i > Int.MinValue + 1 && i < Int.MaxValue - 12) {

                val elems: Seq[Int] = Seq(0, 0, 1, 2, 6, 9, 11).map(_ + i)
                var rt: BTree[Int] = new BTree(
                    Seq(elems(4)), 
                    Seq(
                        new BTree(elems.slice(0, 4), Nil), 
                        new BTree(elems.slice(5, 7), Nil)
                    )
                )
            
                rt.splitChild(0)

                assert(rt.children.length == 3) 
                assert(rt.toString == "((%s,%s),%s,(%s),%s,(%s,%s))".format(
                    elems(0), elems(1), elems(2), elems(3), elems(4), elems(5), elems(6)
                ))
        
            }
        }
        
        /**
          * Split leaf on the right of root
          */
        forAll { (i: Int) =>

            whenever (i > Int.MinValue + 2 && i < Int.MaxValue - 9) {

                val elems: Seq[Int] = Seq(-1, -1, 0, 3, 4, 7, 8).map(_ + i)
                var rt: BTree[Int] = new BTree(
                    Seq(elems(2)), 
                    Seq(
                        new BTree(elems.slice(0, 2), Nil), 
                        new BTree(elems.slice(3, 7), Nil)
                    )
                )

                rt.splitChild(1)

                assert(rt.children.length == 3)
                assert(rt.toString == "((%s,%s),%s,(%s,%s),%s,(%s))".format(
                    elems(0), elems(1), elems(2), elems(3), elems(4), elems(5), elems(6)
                ))

            }
        }
        

    }

    it should "be able to split a full node with singleton root" in {

        forAll { (i: Int) => 

            whenever (i > Int.MinValue + 7 && i < Int.MaxValue - 33) {
                
                val elems: Seq[Int] = Seq(-6, -6, -5, 0, 2, 3, 7, 10, 10, 12, 15, 18, 19, 20, 31, 32).map(_ + i)

                var rt: BTree[Int] = new BTree(
                    Seq(elems(13)), Seq(
                        new BTree(Seq(elems(2), elems(5), elems(8), elems(11)), Seq(
                            new BTree(elems.slice(0, 2), Nil),
                            new BTree(elems.slice(3, 5), Nil),
                            new BTree(elems.slice(6, 8), Nil),
                            new BTree(elems.slice(9, 11), Nil),
                            new BTree(Seq(elems(12)), Nil)
                        )),
                        new BTree(elems.slice(14, 16), Nil)
                    )
                )

                rt.splitChild(0)

                println(rt)

                assert(
                    rt.toString == "(((%s,%s),%s,(%s,%s),%s,(%s,%s)),%s,((%s,%s),%s,(%s)),%s,(%s,%s))".format(
                        elems(0), elems(1), elems(2), elems(3), elems(4), elems(5), elems(6), elems(7), elems(8),
                        elems(9), elems(10), elems(11), elems(12), elems(13), elems(14), elems(15)
                    )
                )

            }
        }
    }

    it should "be able to split a full node with full parent" in {

        forAll { (i: Int) => 

            whenever (i > Int.MinValue + 10 && i < Int.MaxValue - 72) {

                val elems: Seq[Int] = Seq(
                    -9, -8, -6, -6, -5, -2, 0, 2, 2, 3, 4, 5, 7, 10, 10, 12, 15, 15, 16, 18, 18, 19, 19, 20, 20, 31, 32, 
                    35, 36, 40, 41, 45, 50, 55, 59, 60, 65, 70, 71
                ).map(_ + i)
                
                var rt: BTree[Int] = new BTree(
                    Seq(elems(24), elems(29), elems(32), elems(35)), Seq(
                        new BTree(Seq(elems(4), elems(9), elems(14), elems(19)), Seq(
                            new BTree(elems.slice(0, 4), Nil),
                            new BTree(elems.slice(5, 9), Nil),
                            new BTree(elems.slice(10, 14), Nil),
                            new BTree(elems.slice(15, 19), Nil),
                            new BTree(elems.slice(20, 24), Nil)
                        )),
                        new BTree(elems.slice(25, 29), Nil),
                        new BTree(elems.slice(30, 32), Nil),
                        new BTree(elems.slice(33, 35), Nil),
                        new BTree(elems.slice(36, 39), Nil)
                    )
                )
                
                rt.splitChild(0)

                assert(
                    rt.toString == "((((%s,%s,%s,%s),%s,(%s,%s,%s,%s),%s,(%s,%s,%s,%s),%s,(%s,%s,%s,%s),%s,(%s,%s,%s,%s)),%s,(%s,%s,%s,%s),%s,(%s,%s)),%s,((%s,%s),%s,(%s,%s,%s)))".format(
                        elems(0), elems(1), elems(2), elems(3), elems(4), elems(5), elems(6), elems(7), elems(8),
                        elems(9), elems(10), elems(11), elems(12), elems(13), elems(14), elems(15), elems(16),
                        elems(17), elems(18), elems(19), elems(20), elems(21), elems(22), elems(23), elems(24), 
                        elems(25), elems(26), elems(27), elems(28), elems(29), elems(30), elems(31), elems(32), 
                        elems(33), elems(34), elems(35), elems(36), elems(37), elems(38)
                    )
                )

            }
        }
    }

    it should "be able to insert into a non-full leaf" in {

        var rt: BTree[Int] = new BTree(Seq(1, 7, 8), Nil)

        rt.insertNotFull(3)

        assert(rt.keys.length == 4)
        assert(rt.toString == "(1,3,7,8)")

    }

    it should "be able to insert into a non-full node" in {

        var rt: BTree[Int] = new BTree(Seq(9), Seq(
            new BTree(Seq(1, 1, 2, 3), Nil), 
            new BTree(Seq(17, 20, 21, 23), Nil)
        ))
        assert(rt.toString == "((1,1,2,3),9,(17,20,21,23))")

        rt.insertNotFull(12)

        assert(rt.toString == "((1,1,2,3),9,(12,17,20),21,(23))")

    }

}
