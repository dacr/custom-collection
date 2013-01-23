/*
 * Copyright 2012 David Crosson
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dummy

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DummyTest extends FunSuite with ShouldMatchers {
  
  test("custom traversable test") {
    val l = CustomTraversable(1, 2, 3, 4)
    val c = List(5,6,7)    
  
    l should not be equals(List(1,2,3,4))
    
    (l ++ c) should be equals (CustomTraversable(1,2,3,4,5,6,7))
    
    (l.map(_.toString)) should be equals(CustomTraversable("1","2","3","4"))
    
    (l.map(_.toString)).getClass.getName should include ("CustomTraversable")
    
    (l.filter(_ > 2)) should be equals(CustomTraversable(3,4))
    
    l.reduce(_ + _) should equal(10)
  }


  
  test("custom seq test") {
    val l = CustomSeq(1, 2, 3, 4)
    val c = List(5,6,7)    
  
    l should not be equals(List(1,2,3,4))
    
    (l :+ 8) should be equals (CustomSeq(1,2,3,4,8))
    
    (l ++ c) should be equals (CustomSeq(1,2,3,4,5,6,7))
    
    (l.map(_.toString)) should be equals (CustomSeq("1","2","3","4"))
    
    (l.map(_.toString)) should not be equals (IndexedSeq("1","2","3","4"))

    (l.map(_.toString)).getClass.getName should include("CustomSeq")

    (l.filter(_ > 2)) should be equals (CustomSeq(3,4))
    
    l.reduce(_ + _) should equal(10)
  }

  
  
  test("custom seq test 1") {
    val cs = MySeq("1", "2", "3")
    info(cs.toString)
    cs  should be equals (MySeq("1","2","3"))
    
    val scs = cs.map(_.toInt)
    info(scs.toString)
    
    scs  should be equals (MySeq(1,2,3))
    
    scs.getClass.getName should include("MySeq")
  }
  
  
  
  test("custom seq test 2") {
    val cs = NamedSeq("toto", "1", "2", "3")
    info(cs.toString)
    
    cs  should be equals (NamedSeq("toto", "1","2","3"))
    
    val scs = cs.map(_.toInt)
    info(scs.toString)
    
    scs  should be equals (NamedSeq("toto", 1,2,3))
    
    scs.getClass.getName should include("NamedSeq")
  }
  
  
  
  test("custom seq test 3") {
    val cs  = MySeq(5,6,7,8)
    val scs = cs.filter(_ > 6)
    
    val ncs  = NamedSeq("myseq", 1,2,3,4)
    val nscs = ncs.filter(_ > 2)
    
    (nscs :+ 10)      should be equals(NamedSeq("myseq", 3,4,10))
    
    (nscs ++ scs)     should be equals(NamedSeq("myseq", 3,4,7,8))
    
    (scs ++ nscs)     should be equals(MySeq(7,8,3,4))
    
    (nscs.map(_ + 1)) should be equals(NamedSeq("myseq",4,5))
    
    (nscs.map(_.toString))  should be equals(NamedSeq("myseq","3","4"))

    (scs.map(_.toString))  should be equals(MySeq("7","8"))
  }
  
}
