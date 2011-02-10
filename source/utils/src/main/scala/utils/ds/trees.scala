/*
 * Trees.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */


package at.logic.utils.ds
import at.logic.utils.logging.Logger

import acyclicGraphs._
import graphs._
import scala.collection.JavaConversions._

package trees {
  /**
   * Trees are constructed over the inductive graph type and are characterised by vertices not repeating in the subtrees.
   * Important!! The notion of repeated vertex is according to the vertex equality, i.e. two different instances of int vertices will be considered equal if they are both the same number.
   *
   */

  trait Tree[+V] extends AGraph[V] {
    require {isTree} // important, read remark above
    protected def isTree: Boolean // TODO optimize isTree (in binaryTree)
    val vertex: V
    def name: String // used to contain more information about the tree, like rule names in LK
    def fold[T](leafF: V => T)(unaryF: (T, V) => T)(binaryF: (T,T,V)=> T): T
  }

  class LeafTree[+V](override val vertex: V) extends LeafAGraph[V](vertex) with Tree[V] {
    protected def isTree: Boolean = true // any leaf is a tree
    def fold[T](leafF: V => T)(unaryF: (T, V) => T)(binaryF: (T,T,V)=> T): T = leafF(vertex)
  }
  object LeafTree {
    def apply[V](vertex: V) = new LeafTree[V](vertex)
    def unapply[V](t: Tree[V]) = t match {
      case t: LeafTree[_] => Some(t.vertex)
      case t: Tree[_] => None
    }
  }

  class UnaryTree[+V](override val vertex: V, override val t: Tree[V]) extends UnaryAGraph[V](vertex, t) with Tree[V] {
    protected def isTree: Boolean = true // any unary tree is a tree if its child component is a tree
    def fold[T](leafF: V => T)(unaryF: (T, V) => T)(binaryF: (T,T,V)=> T): T = unaryF(t.fold(leafF)(unaryF)(binaryF), vertex)
  }
  object UnaryTree {
    def apply[V](vertex: V, t: Tree[V]) = new UnaryTree[V](vertex, t)
    def unapply[V](t: Tree[V]) = t match {
      case t: UnaryTree[_] => Some((t.vertex, t.t))
      case t: Tree[_] => None
    }
  }
  class BinaryTree[+V](override val vertex: V, override val t1: Tree[V], override val t2: Tree[V]) extends BinaryAGraph[V](vertex,t1,t2) with Tree[V] {
    protected def isTree: Boolean = {
      // we must check that no subtree in one sub component is equal to a subtree in the other component
      // it is enough to check leaves only
      val axs: Set[V] = t2.fold(v => Set(v))((s,_) => s)((s1,s2,_) => s1++s2) // create a set of all elements in leafs in the second tree
      !t1.fold(v => axs.contains(v))((b,_) => b)((b1,b2,_) => b1 | b2)
    }
    def fold[T](leafF: V => T)(unaryF: (T, V) => T)(binaryF: (T,T,V)=> T): T = binaryF(t1.fold(leafF)(unaryF)(binaryF), t2.fold(leafF)(unaryF)(binaryF), vertex)
  }
  object BinaryTree {
    def apply[V](vertex: V, t1: Tree[V], t2: Tree[V]) = new BinaryTree[V](vertex, t1, t2)
    def unapply[V](t: Tree[V]) = t match {
      case t: BinaryTree[_] => Some((t.vertex, t.t1, t.t2))
      case t: Tree[_] => None
    }
  }

  /*class ArbitraryTree[+V] private (override val vertex: V, override val lastParent: Tree[V], override val restParents: List[Tree[V]], graph: Graph[V]) extends ArbitraryAGraph[V](vertex,lastParent,restParents,graph) with Tree[V]
  // TODO add a require so it remains a tree (check no vertex repeats and new vertex is new)
  object ArbitraryTree extends Logger {
    def apply[V](vertex: V, parents: Tree[V]*) = {val ls = parents.toList; ls match {
      case Nil => LeafTree[V](vertex)
      case t::Nil => UnaryTree[V](vertex, t)
      case t1::t2::Nil => BinaryTree[V](vertex, t1, t2)
      case t::tls => applyRec[V](vertex, tls, ls, EdgeGraph[V](t.vertex, vertex, VertexGraph[V](vertex, t)))
    }}
    def applyRec[V](vertex: V, trees: List[Tree[V]], allParents: List[Tree[V]], graph: Graph[V]): ArbitraryTree[V] = trees match {
      case Nil => error("The recursive call in arbitrary tree is always called on at least two arguments as the other cases are being handled by unary and binary trees", new AssertionError())
      case t::Nil => new ArbitraryTree[V](vertex, allParents.head, allParents.tail, graph)
      case t::tls => applyRec[V](vertex, tls, allParents, EdgeGraph[V](t.vertex, vertex, UnionGraph[V](graph, t)))
    }
    def unapply[V](t: Tree[V]) = t match {
      case t: ArbitraryTree[_] => Some((t.vertex, (t.lastParent::t.restParents)))
      case t: Tree[_] => None
    }
  }  */

  object TreeImplicitConverters {
    implicit def toLeafTree[V](v:V): LeafTree[V] = LeafTree[V](v)
    implicit def toUnaryTree[V](pair: Tuple2[V, Tree[V]]): UnaryTree[V] = UnaryTree[V](pair._1, pair._2)
    implicit def toBinaryTree[V](triple: Tuple3[V, Tree[V], Tree[V]]): BinaryTree[V] = BinaryTree[V](triple._1, triple._2, triple._3)
  }
}