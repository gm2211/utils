package com.gm2211.collections

/**
  * Gets rid of annoying deprecation warning: the scala lib says the stack implementation is inefficient, but then they
  * tell you to basically perform the same operations explicitly.
  *
  * For example they recommend the following:
  *
  * var stack: List[Int] = List()
  *
  * // push 1
  * stack = 1 :: stack
  * // push 2
  * stack = 2 :: stack
  *
  * // pop 2
  * val popped = stack.head
  * stack = stack.tail
  *
  * // pop 1
  * val popped = stack.head
  * stack = stack.tail
  *
  *
  * However, that is exactly how Stack works internally and is much more verbose. This trait contains a subset of the
  * Stack's methods and is not iterable, but it re-introduces a more user-friendly stack without deprecation warnings.
  */
trait Stack[T] {
  def empty: Boolean
  def nonEmpty: Boolean
  def push(elem: T): Unit
  def peek(): T
  def pop(): T
}

class ListBasedStack[T] extends Stack[T] {
  private var elems: List[T] = List()

  def empty: Boolean = elems.isEmpty
  def nonEmpty: Boolean = elems.nonEmpty
  def push(elem: T): Unit = elems.synchronized { elems = elem :: elems }
  def peek(): T = elems.head
  def pop(): T = elems.synchronized {
    val head = peek()
    elems = elems.tail
    head
  }
}

object Stack {
  def apply[T](): Stack[T] = new ListBasedStack[T]()
}
