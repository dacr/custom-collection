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
  
  test("Simple test") {
    val cs  = CustomSeq(1,2,3,4)
    val scs = cs.filter(_ > 2)
    println(scs.mkString(" "))
    
    val ncs  = NamedSeq("my named seq", 1,2,3,4)
    val nscs = ncs.filter(_ > 2)
    
    nscs.isInstanceOf[NamedSeq[Int]] should be (true)
    
    (nscs :+ 10).isInstanceOf[NamedSeq[Int]] should be (true)
    (nscs ++ scs).isInstanceOf[NamedSeq[Int]] should be (true)
    (scs ++ nscs).isInstanceOf[CustomSeq[Int]] should be (true)
    (nscs.map(_ + 1)).isInstanceOf[NamedSeq[Int]] should be (true)
    
    val tmp = (nscs.map(i => s"(${i})"))
    println(tmp.getClass().getName())
    tmp.isInstanceOf[NamedSeq[String]] should be (true)
  }
  
  
  test("Second test") {
    val cs = NamedSeq("toto", "1", "2", "3")
    cs.isInstanceOf[NamedSeq[String]] should be (true)
    
    val scs = cs.map(_.toInt)
    scs.isInstanceOf[NamedSeq[Int]] should be (true)
  }
}
