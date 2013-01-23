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

import scala.collection.IndexedSeqLike
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom


// ================================ CustomSeq ==================================

object CustomSeq {

  def apply[Base](bases: Base*) = fromSeq(bases)

  def fromSeq[Base](buf: Seq[Base]): CustomSeq[Base] = {
    var array = new ArrayBuffer[Base](buf.size)
    for (i <- 0 until buf.size) array += buf(i)
    new CustomSeq[Base](array)
  }

  def newBuilder[Base]: Builder[Base, CustomSeq[Base]] =
    new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom[Base]: CanBuildFrom[CustomSeq[Base], Base, CustomSeq[Base]] =
    new CanBuildFrom[CustomSeq[Base], Base, CustomSeq[Base]] {
      def apply(): Builder[Base, CustomSeq[Base]] = newBuilder
      def apply(from: CustomSeq[Base]): Builder[Base, CustomSeq[Base]] = newBuilder
    }
}

class CustomSeq[Base] protected (
  //  name: String,
  buffer: ArrayBuffer[Base])
  extends IndexedSeq[Base] with IndexedSeqLike[Base, CustomSeq[Base]] {
  
  override protected[this] def newBuilder: Builder[Base, CustomSeq[Base]] = CustomSeq.newBuilder

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

  implicit def canBuildFrom[Base]: CanBuildFrom[NamedSeq[Base], Base, NamedSeq[Base]] =
    new CanBuildFrom[NamedSeq[Base], Base, NamedSeq[Base]] {
      def apply(): Builder[Base, NamedSeq[Base]] = newBuilder("") // TODO ??
      def apply(from: NamedSeq[Base]): Builder[Base, NamedSeq[Base]] = newBuilder(from.name)
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
