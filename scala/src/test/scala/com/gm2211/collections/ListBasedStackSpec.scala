package com.gm2211.collections

import org.scalatest.FlatSpec

final class ListBasedStackSpec extends FlatSpec {

  "An empty stack" should "return empty" in {
    val stack = Stack[Int]()

    assert(stack.empty)
    assert(!stack.nonEmpty)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = Stack[Int]()
    assertThrows[NoSuchElementException] {
      emptyStack.pop()
    }
  }

  "A stack" should "return elements in LIFO order" in {
    val stack = Stack[Int]()

    stack.push(1)
    stack.push(2)

    assert(stack.pop() == 2)
    assert(stack.pop() == 1)
  }

  it should "not be empty after pushing an element to it" in {
    val stack = Stack[Int]()

    stack.push(1)

    assert(stack.nonEmpty)
    assert(!stack.empty)
  }
  
  it should "be empty after popping all items" in {
    val stack = Stack[Int]()

    stack.push(1)
    stack.pop()

    assert(stack.empty)
    assert(!stack.nonEmpty)
  }
}
