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

import scala.collection._
import scala.collection.mutable.{ArrayBuffer,ListBuffer, Builder}
import scala.collection.generic._
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.SetBuilder

import scala.language.postfixOps


// ================================ CustomTraversable ==================================

object CustomTraversable extends TraversableFactory[CustomTraversable] {  
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, CustomTraversable[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A] = new ListBuffer[A] mapResult (x => new CustomTraversable(x:_*))
}

class CustomTraversable[A](seq : A*) 
     extends Traversable[A]
     with GenericTraversableTemplate[A, CustomTraversable]
     with TraversableLike[A, CustomTraversable[A]] {
  
  override def companion = CustomTraversable
  override def foreach[U](f: A => U) = seq.foreach(f)
}


// ================================ CustomSeq ==================================

object CustomSeq extends SeqFactory[CustomSeq] {  
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, CustomSeq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A] = new ListBuffer[A] mapResult (x => new CustomSeq(x:_*))
}

class CustomSeq[A](seq : A*) 
     extends Seq[A]
     with GenericTraversableTemplate[A, CustomSeq]
     with SeqLike[A, CustomSeq[A]] {
  
  override def companion = CustomSeq
  def iterator: Iterator[A] = seq.iterator
  def apply(idx: Int): A = {
    if (idx < 0 || idx>=length) throw new IndexOutOfBoundsException
    seq(idx)
  }
  def length: Int = seq.size
}



// ================================ CustomSeq ==================================

object MySeq {

  def apply[Base](bases: Base*) = fromSeq(bases)

  def fromSeq[Base](buf: Seq[Base]): MySeq[Base] = {
    var array = new ArrayBuffer[Base](buf.size)
    for (i <- 0 until buf.size) array += buf(i)
    new MySeq[Base](array)
  }

  def newBuilder[Base]: Builder[Base, MySeq[Base]] =
    new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom[Base,From]: CanBuildFrom[MySeq[_], Base, MySeq[Base]] =
    new CanBuildFrom[MySeq[_], Base, MySeq[Base]] {
      def apply(): Builder[Base, MySeq[Base]] = newBuilder
      def apply(from: MySeq[_]): Builder[Base, MySeq[Base]] = newBuilder
    }
}


class MySeq[Base] protected (buffer: ArrayBuffer[Base])
       extends IndexedSeq[Base]
       with IndexedSeqLike[Base, MySeq[Base]] {
  
  override protected[this] def newBuilder: Builder[Base, MySeq[Base]] = MySeq.newBuilder

  def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
    buffer(idx)
  }

  def length = buffer.length

}

// ================================ NamedSeq ==================================
object NamedSeq {

  def apply[Base](name: String, bases: Base*) = fromSeq(name, bases)

  def fromSeq[Base](name: String, buf: Seq[Base]): NamedSeq[Base] = {
    var array = new ArrayBuffer[Base](buf.size)
    for (i <- 0 until buf.size) array += buf(i)
    new NamedSeq[Base](name, array)
  }

  def newBuilder[Base](name: String): Builder[Base, NamedSeq[Base]] =
    new ArrayBuffer mapResult { x: ArrayBuffer[Base] => fromSeq(name, x) }

  implicit def canBuildFrom[Base]: CanBuildFrom[NamedSeq[_], Base, NamedSeq[Base]] =
    new CanBuildFrom[NamedSeq[_], Base, NamedSeq[Base]] {
      def apply(): Builder[Base, NamedSeq[Base]] = newBuilder("default")
      def apply(from: NamedSeq[_]): Builder[Base, NamedSeq[Base]] = newBuilder(from.name)
    }
}

class NamedSeq[Base] protected (
  val name: String,
  buffer: ArrayBuffer[Base])
  extends IndexedSeq[Base] with IndexedSeqLike[Base, NamedSeq[Base]] {

  override protected[this] def newBuilder: Builder[Base, NamedSeq[Base]] = NamedSeq.newBuilder(name)

  def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
    buffer(idx)
  }

  def length = buffer.length
  
  override def toString() = "NamedSeq("+name+" : "+mkString(", ")+")"
}

// ============================= CustomVector ===================================

object CustomVector {

  def apply[Base](bases: Base*) = fromSeq(bases.toVector)

  def fromSeq[Base](buf: Vector[Base]): CustomVector[Base] = new CustomVector[Base](buf)

  def newBuilder[Base]: Builder[Base, CustomVector[Base]] =
    new VectorBuilder mapResult fromSeq

  implicit def canBuildFrom[Base,From]: CanBuildFrom[CustomVector[_], Base, CustomVector[Base]] =
    new CanBuildFrom[CustomVector[_], Base, CustomVector[Base]] {
      def apply(): Builder[Base, CustomVector[Base]] = newBuilder
      def apply(from: CustomVector[_]): Builder[Base, CustomVector[Base]] = newBuilder
    }
}


class CustomVector[Base] protected (buffer: Vector[Base])
       extends IndexedSeq[Base]
       with IndexedSeqLike[Base, CustomVector[Base]] {
  
  override protected[this] def newBuilder: Builder[Base, CustomVector[Base]] = CustomVector.newBuilder

  def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
    buffer(idx)
  }

  def length = buffer.length

}


// ============================= CustomSet ===================================
object CustomSet {//}extends ImmutableSetFactory[CustomSet] {
  
  def apply[A](s:A*) = new CustomSet(s.toSet)
  
  implicit def canBuildFrom[A,B]: CanBuildFrom[CustomSet[B], A, CustomSet[A]] = new CanBuildFrom[CustomSet[B], A, CustomSet[A]] {
    def apply(from: CustomSet[B]) = newBuilder
    def apply() = newBuilder
  }
  def empty[A] = new CustomSet[A]()
  def newBuilder[A]: Builder[A, CustomSet[A]] =  new SetBuilder[A, CustomSet[A]](empty)
}


class CustomSet[A] protected(backend:Set[A]=Set.empty[A])
  extends Set[A]
  with SetLike[A, CustomSet[A]] {
  
  def contains(key: A): Boolean = backend.contains(key)
  def iterator: Iterator[A] = backend.iterator
  def +(elem: A)  = if ( contains(elem)) this else new CustomSet(backend+elem)
  def -(elem: A)  = if (!contains(elem)) this else new CustomSet(backend-elem)
  override def empty = CustomSet.empty
}


// ============================= NamedCustomSet ===================================
object NamedCustomSet {
  
  def apply[A](name:String, s:A*) = new NamedCustomSet(name, s.toSet)
  
  implicit def canBuildFrom[A,B]: CanBuildFrom[NamedCustomSet[B], A, NamedCustomSet[A]] = 
    new CanBuildFrom[NamedCustomSet[B], A, NamedCustomSet[A]] {
      def apply(from: NamedCustomSet[B]) = newBuilder(from.name)
      def apply() = newBuilder
  }
  def empty[A] = new NamedCustomSet[A]("empty")
  
  def newBuilder[A]: Builder[A, NamedCustomSet[A]] =  
    new SetBuilder[A, NamedCustomSet[A]](empty)
    
  def newBuilder[A](name:String): Builder[A, NamedCustomSet[A]] =  
    new SetBuilder[A, NamedCustomSet[A]](new NamedCustomSet[A](name))
}


class NamedCustomSet[A](val name:String, protected val backend:Set[A]=Set.empty[A])
  extends Set[A]
  with SetLike[A, NamedCustomSet[A]] {
  
  def contains(key: A): Boolean = backend.contains(key)
  def iterator: Iterator[A] = backend.iterator
  def +(elem: A)  = if ( contains(elem)) this else new NamedCustomSet(name, backend+elem)
  def -(elem: A)  = if (!contains(elem)) this else new NamedCustomSet(name, backend-elem)
  override def empty = NamedCustomSet.empty
  override def toString = {
    val content = size match {
      case 0 => ""
      case x if x < 10 => mkString(",",",","")
      case _ => take(9).mkString(",",",","...")
    }
    "NamedCustomSet("+name+content+")"
  }
}


// ============================= CustomMap ===================================

/*
object CustomMap extends ImmutableMapFactory[CustomMap] {
  override def empty[KEY, VAL]: Coll = new CustomMap[KEY,VAL]()
}

class CustomMap[KEY,VAL] protected() 
   extends Map[KEY,VAL]
   with MapLike[KEY,VAL,CustomMap[KEY,VAL]]  {
  
}
*/

// ===========================================================================
object Dummy {
  def main(args: Array[String]):Unit = {

  }
}




// ===========================================================================

case class Thing()

import scala.collection.SetLike
import scala.collection.generic.{GenericSetTemplate, GenericCompanion, CanBuildFrom}
import scala.collection.mutable.{Builder, SetBuilder}

class ThingSet(seq : Thing*) extends Set[Thing] 
                             with SetLike[Thing, ThingSet] {
    override def empty: ThingSet = new ThingSet()
    def + (elem: Thing) : ThingSet = if (seq contains elem) this 
        else new ThingSet(elem +: seq: _*)
    def - (elem: Thing) : ThingSet = if (!(seq contains elem)) this
        else new ThingSet(seq filterNot (elem ==): _*)
    def contains (elem: Thing) : Boolean = seq exists (elem ==)
    def iterator : Iterator[Thing] = seq.iterator
}

object ThingSet {
    def empty: ThingSet = new ThingSet()
    def newBuilder: Builder[Thing, ThingSet] = new SetBuilder[Thing, ThingSet](empty)
    def apply(elems: Thing*): ThingSet = (empty /: elems) (_ + _)
    def thingSetCanBuildFrom = new CanBuildFrom[ThingSet, Thing, ThingSet] {
        def apply(from: ThingSet) = newBuilder
        def apply() = newBuilder
    }
}

// ===========================================================================


import scala.collection.SetLike
import scala.collection.generic.{GenericSetTemplate, GenericCompanion, CanBuildFrom}
import scala.collection.mutable.{Builder, SetBuilder}

class DummySet[A](val name:String, set : Set[A]) extends Set[A] 
                             with SetLike[A, DummySet[A]] {

  def +(elem: A): DummySet[A] = new DummySet[A](name, set + elem)
  def -(elem: A): DummySet[A] = new DummySet[A](name, set - elem)
  def contains(elem: A): Boolean = set contains elem
  def iterator: Iterator[A] = set.iterator

  override def companion = DummySet
  override def empty: DummySet[A] = DummySet.empty[A]
  // Required for `filter`, `take`, `drop`, etc. to preserve `someProperty`.
  override def newBuilder: mutable.Builder[A, DummySet[A]] = 
    DummySet.newBuilder[A](name)
}


object DummySet extends SetFactory[DummySet] {
  class DummySetBuilder[A](name: String) extends 
    mutable.SetBuilder[A, DummySet[A]](new DummySet(name, Set.empty))

  def newBuilder[A]:DummySetBuilder[A] = newBuilder[A]("")
  def newBuilder[A](name: String) = new DummySetBuilder[A](name)

  override def empty[A]= new DummySet[A]("", Set.empty)
  def apply[A](name:String) = new DummySet[A](name, Set.empty)

  implicit def canBuildFrom[A]: CanBuildFrom[DummySet[_], A, DummySet[A]] =
    new CanBuildFrom[DummySet[_], A, DummySet[A]] {
      def apply(from: DummySet[_]): mutable.Builder[A, DummySet[A]] = newBuilder[A](from.name)
      def apply(): mutable.Builder[A, DummySet[A]] = newBuilder[A]
    }
}




// ===========================================================================

// grrrr : Set are invariant !!! no covariance ! so no way to make this work !!!


/*
trait Cell {
  val time:Long
  val value:Double
}

class DummyCellSet[+A <: Cell](val name:String, set : Set[A]) extends Set[A] 
                             with SetLike[A, DummyCellSet[A]] {

  def +(elem: A): DummyCellSet[A] = new DummyCellSet[A](name, set + elem)
  def -(elem: A): DummyCellSet[A] = new DummyCellSet[A](name, set - elem)
  def contains(elem: A): Boolean = set contains elem
  def iterator: Iterator[A] = set.iterator

  override def companion = DummyCellSet
  override def empty: DummyCellSet[A] = DummyCellSet.empty[A]
  // Required for `filter`, `take`, `drop`, etc. to preserve `someProperty`.
  override def newBuilder: mutable.Builder[A, DummyCellSet[A]] = 
    DummyCellSet.newBuilder[A](name)
}


object DummyCellSet extends SetFactory[DummyCellSet] {
  class DummyCellSetBuilder[A](name: String) extends 
    mutable.SetBuilder[A, DummyCellSet[A]](new DummyCellSet(name, Set.empty))

  def newBuilder[A]:DummyCellSetBuilder[A] = newBuilder[A]("")
  def newBuilder[A](name: String) = new DummyCellSetBuilder[A](name)

  override def empty[A]= new DummyCellSet[A]("", Set.empty)
  def apply[A](name:String) = new DummyCellSet[A](name, Set.empty)

  implicit def canBuildFrom[A]: CanBuildFrom[DummyCellSet[_], A, DummyCellSet[A]] =
    new CanBuildFrom[DummyCellSet[_], A, DummyCellSet[A]] {
      def apply(from: DummyCellSet[_]): mutable.Builder[A, DummyCellSet[A]] = newBuilder[A](from.name)
      def apply(): mutable.Builder[A, DummyCellSet[A]] = newBuilder[A]
    }
}
*/



// ===========================================================================

trait Cell {
  val time:Long
  val value:Double
}

class BaseCell(
  val time:Long,
  val value:Double
)

class StatCell(
  time:Long,
  value:Double,
  val avg:Double
) extends BaseCell(time, value)



object DummyCellColl extends SeqFactory[DummyCellColl] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, DummyCellColl[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A] = new ListBuffer[A] mapResult (x => new DummyCellColl(x:_*))
}

class DummyCellColl[+A](seq : A*) 
     extends Seq[A]
     with GenericTraversableTemplate[A, DummyCellColl]
     with SeqLike[A, DummyCellColl[A]] {
  
  override def companion = DummyCellColl
  def iterator: Iterator[A] = seq.iterator
  def apply(idx: Int): A = {
    if (idx < 0 || idx>=length) throw new IndexOutOfBoundsException
    seq(idx)
  }
  def length: Int = seq.size
}
