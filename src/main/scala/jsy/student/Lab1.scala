package jsy.student

import jsy.lab1._

object Lab1 extends jsy.util.JsyApplication with jsy.lab1.Lab1Like {
  import jsy.lab1.Parser
  import jsy.lab1.ast._

  /**
   * CSCI 3155: Lab 1
   * Jarrod Raine
   *
   * Partner: Jasmine Rethmann
   * Collaborators: N/A
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  /*
   * Example: Test-driven development of plus
   *
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter. The simplest way
   * to use the interactive Scala interpreter in IntelliJ is through a worksheet,
   * such as Lab1Worksheet.sc. A Scala Worksheet (e.g., Lab1Worksheet.sc) is code
   * evaluated in the context of the project with results for each expression
   * shown inline.
   *
   * Step 0: Sketch an implementation in Lab1.scala using ??? for unimmplemented things.
   * Step 1: Do some experimentation in Lab1Worksheet.sc.
   * Step 2: Write a test in Lab1Spec.scala, which should initially fail because of the ???.
   * Step 3: Fill in the ??? to finish the implementation to make your test pass.
   */

  //def plus(x: Int, y: Int): Int = ???
  def plus(x: Int, y: Int): Int = x + y

  /* Exercises */

  //If the value is negative, return the positive (-n). Otherwise, return n.
  def abs(n: Double): Double = if (n < 0) -n else n

  //If a is true and b is false or if a is false and b is true, return true
  def xor(a: Boolean, b: Boolean): Boolean = if ((a && !b) || (!a && b)) true else false

  //Turns out this beats the instructor solution as you can multiply strings in Scala
  def repeat(s: String, n: Int): String = {
    require(n >= 0)
    s*n
  }

  //Newton's Method for Calculating Square Roots
  def sqrtStep(c: Double, xn: Double): Double = xn - ((xn * xn - c) / (2.0 * xn))

  //Call sqrtStep n times
  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    require(n >= 0)
    if (n == 0)
      x0
    else
      sqrtN(c,sqrtStep(c,x0),n-1)
  }

  //Keep calculating the square root until the error is smaller than epsilon
  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    require(epsilon > 0)
    if (abs(x0*x0 - c) < epsilon) x0 else sqrtErr(c,sqrtStep(c,x0),epsilon)
  }

  //Default Square root calculation
  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }

  /* Search Tree */

  // Defined in Lab1Like.scala:
  //
  // sealed abstract class SearchTree
  // case object Empty extends SearchTree
  // case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree

  /* FIX EXPLANATION
   */
  def repOk(t: SearchTree): Boolean = {
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      case Empty => true
      case Node(l, d, r) => check(l,min,d) && check(r, d, max) && (d >= min) && (d < max)
    }
    check(t, Int.MinValue, Int.MaxValue)
  }

  /* All we have to do is figure out where to insert the node and then add it
   * as soon as we find a valid empty space. As such, in the case that we are
   * still traversing between nodes, we check of the value we are adding against
   * the value of the current node. If the value to insert is greater than or equal
   * to the current node value, we move to the right. Otherwise, we move left.
   * Once we reach an empty space, we add a new node with that value.
   */
  def insert(t: SearchTree, n: Int): SearchTree = t match {
    case Empty => Node(Empty,n,Empty)
    case Node(l,d,r) => {
      if (n >= d)
        Node(l,d,insert(r,n))
      else
        Node(insert(l,n),d,r)
    }
  }

  /* The purpose of deleteMin is to delete the minimum value that occurs in the
   * right subtree of the current node and return the updated subtree as well as
   * the value of that deleted node.
   *
   * As such, we require the right subtree to not be empty as well as that the
   * subtree has not been checked thus far. Now, there are 2 cases to consider.
   *
   * For the first case, we have a subtree node that only has a right child. This
   * means that we have found the minimum value, for which we return the search tree
   * defined with the right child as the root and the value of that node.
   *
   * For the second case, we have a node that has a left child (Whether or not
   * there is a right child is irrelevant for this). As such, we then have to
   * recursively use our algorithm on that left subtree until we reach the minimum
   * value. Then, we return the corresonding subtree as well as the minimum value.
   */
  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d)
      case Node(l, d, r) => {
        val (ll, m) = deleteMin(l)
        (Node(ll,d,r),m)
      }
    }
  }

  /* All we have to do is traverse the tree to locate the value to be deleted.
   * As such, we have several cases depending on our recursive function.
   *
   * For the first case, we come across an "empty" node (i.e. the value doesnt
   * exist), to which we return Empty as it means we're already off the tree.
   *
   * For the second case, we are at a leaf node. If that node does contain the
   * value we are looking for, we delete the node as is by returning "Empty".
   * However, if that leaf node doesn't contain our value, we keep the node as is
   * because our value isn't in the tree.
   *
   * For the third and fourth cases, we have a node that has exactly 1 child. To
   * explain, I use the third case, which assumes the node only has a right child.
   * If the value we are looking for is greater than than the current node value,
   * we move to the right child. If the value is less than the current node, then
   * the value doesn't exist. Finally, if the value is the same as the current node,
   * we simply return the right node to have it replace the current node. Reverse
   * the concepts to apply the logic to a node that only has a left child.
   *
   * For the fifth case, we have a node with 2 children. Once again, we traverse
   * the tree if the value doesnt match the node. However, if it does match, then
   * we call deleteMin() on the right side to pull that minimum value there and
   * replace the current node with that node as well as move nodes around accordingly
   * to account for the moved minimum right node from the node we just deleted.
   */
  def delete(t: SearchTree, n: Int): SearchTree = t match {
    case Empty => Empty
    case Node(Empty,d,Empty) => if(n == d) Empty else Node(Empty,d,Empty)
    case Node(Empty,d,r) => {
      if (n > d) Node(Empty,d,delete(r,n))
      else if (n < d) Node(Empty,d,r)
      else r
    }
    case Node(l,d,Empty) => {
      if (n > d) Node(l,d,Empty)
      else if (n < d) Node(delete(l,n),d,Empty)
      else l
    }
    case Node(l,d,r) => {
      if (n > d) Node(l,d,delete(r,n))
      else if (n < d) Node(delete(l,n),d,r)
      else {
        val (rr,m) = deleteMin(r)
        Node(l,m,rr)
      }
    }
  }

  /* JavaScripty */

  /* This is actually pretty easy. We just have to implement the Unary and Binary
   * operations for negative, plus, minus, times, and div for our expressions. When
   * they are all "isolated" (i.e. eval(e1) or eval(e2)), then we simply return that
   * expression as a number.
   */
  def eval(e: Expr): Double = e match {
    case N(n) => n
    case Unary(Neg, e1) => if(eval(e1) >= 0) -eval(e1) else eval(e1)
    case Binary(Plus, e1, e2) => eval(e1) + eval(e2)
    case Binary(Minus, e1, e2) => eval(e1) - eval(e2)
    case Binary(Times, e1, e2) => eval(e1) * eval(e2)
    case Binary(Div, e1, e2) => eval(e1) / eval(e2)
    case _ => throw new UnsupportedOperationException
  }

 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */

 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(prettyNumber(v))
  }

}
