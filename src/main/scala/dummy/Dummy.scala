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
  def apply(idx: Int): A = if (idx>length) throw new IndexOutOfBoundsException else seq(idx)
  def length: Int = seq.size
}



// ================================ CustomSeq ==================================

object CustomSeq2 {

  def apply[Base](bases: Base*) = fromSeq(bases)

  def fromSeq[Base](buf: Seq[Base]): CustomSeq2[Base] = {
    var array = new ArrayBuffer[Base](buf.size)
    for (i <- 0 until buf.size) array += buf(i)
    new CustomSeq2[Base](array)
  }

  def newBuilder[Base]: Builder[Base, CustomSeq2[Base]] =
    new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom[Base,From]: CanBuildFrom[CustomSeq2[_], Base, CustomSeq2[Base]] =
    new CanBuildFrom[CustomSeq2[_], Base, CustomSeq2[Base]] {
      def apply(): Builder[Base, CustomSeq2[Base]] = newBuilder
      def apply(from: CustomSeq2[_]): Builder[Base, CustomSeq2[Base]] = newBuilder
    }
}

class CustomSeq2[Base] protected (buffer: ArrayBuffer[Base])
       extends IndexedSeq[Base]
       with IndexedSeqLike[Base, CustomSeq2[Base]] {
  
  override protected[this] def newBuilder: Builder[Base, CustomSeq2[Base]] = CustomSeq2.newBuilder

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
    new NamedSeq[Base](name: String, array)
  }

  def newBuilder[Base](name: String): Builder[Base, NamedSeq[Base]] =
    new ArrayBuffer mapResult { x: ArrayBuffer[Base] => fromSeq("", x) }

  implicit def canBuildFrom[Base]: CanBuildFrom[NamedSeq[_], Base, NamedSeq[Base]] =
    new CanBuildFrom[NamedSeq[_], Base, NamedSeq[Base]] {
      def apply(): Builder[Base, NamedSeq[Base]] = newBuilder("") // TODO ??
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
}



// ===========================================================================
object Dummy {
  def main(args: Array[String]) {

  }
}
