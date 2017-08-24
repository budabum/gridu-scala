package patmat

import common._

/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {

  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
    * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
    * leaves.
    */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(left, right, chars, w) => weight(left) + weight(right)
    case Leaf(char, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(left, right, cs, w) => chars(left) ++ chars(right)
    case Leaf(char, w) => List(char)
  }

  def left(tree: CodeTree): CodeTree = tree match {
    case Fork(leftTree, right, chars, weight) => leftTree
    case Leaf(_, _) => throw new UnsupportedOperationException("Leaf.left")
  }

  def right(tree: CodeTree): CodeTree = tree match {
    case Fork(left, rightTree, chars, weight) => rightTree
    case Leaf(_, _) => throw new UnsupportedOperationException("Leaf.right")
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
    * In this assignment, we are working with lists of characters. This function allows
    * you to easily create a character list from a given string.
    */
  def string2Chars(str: String): List[Char] = str.toList

  /**
    * This function computes for each unique character in the list `chars` the number of
    * times it occurs. For example, the invocation
    *
    * times(List('a', 'b', 'a'))
    *
    * should return the following (the order of the resulting list is not important):
    *
    * List(('a', 2), ('b', 1))
    *
    * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
    * character and an integer. Pairs can be constructed easily using parentheses:
    *
    * val pair: (Char, Int) = ('c', 1)
    *
    * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
    *
    * val theChar = pair._1
    * val theInt  = pair._2
    *
    * Another way to deconstruct a pair is using pattern matching:
    *
    * pair match {
    * case (theChar, theInt) =>
    * println("character is: "+ theChar)
    * println("integer is  : "+ theInt)
    * }
    */
  def times(chars: List[Char]): List[(Char, Int)] = {
    chars.groupBy(identity).mapValues(s => s.size).toList
  }

  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.sortBy(_._2).map(e => Leaf(e._1, e._2))
  }

  /**
    * Checks whether the list `trees` contains only one single code tree.
    */
  def singleton(trees: List[CodeTree]): Boolean = {
    trees.size == 1
  }

  /**
    * The parameter `trees` of this function is a list of code trees ordered
    * by ascending weights.
    *
    * This function takes the first two elements of the list `trees` and combines
    * them into a single `Fork` node. This node is then added back into the
    * remaining elements of `trees` at a position such that the ordering by weights
    * is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be returned
    * unchanged.
    */
  def combine1(trees: List[CodeTree]): List[CodeTree] = {
    def f(t: List[CodeTree], acc: List[CodeTree]): List[CodeTree] = {
      println(s"F:IN: $t")
      val h1 = t.head
      if (t.tail.isEmpty) {
        h1 :: acc
      }
      else {
        val h2 = t.tail.head
        val tt = t.tail.tail
        val combinedLeaf = Fork(h1, h2, (chars(h1) ++ chars(h2)).distinct, weight(h1) + weight(h2))
        val res = combinedLeaf :: acc
        if (tt.isEmpty) res
        else f(tt, res)
      }
    }

    println(s"TREES: $trees")
    if (singleton(trees) || trees.isEmpty) {
      trees
    }
    else {
      f(trees, List())
    }
  }

  def combine2(trees: List[CodeTree]): List[CodeTree] = {
    if (singleton(trees) || trees.isEmpty) trees
    else {
      val h2 = trees.head
      val h1 = trees.tail.head
      val combinedLeaf = Fork(h1, h2, (chars(h1) ++ chars(h2)).distinct, weight(h1) + weight(h2))
      combinedLeaf :: trees.tail.tail
    }
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    if (singleton(trees) || trees.isEmpty) trees
    else {
      val h1 = trees.head
      val h2 = trees.tail.head
      val combinedLeaf = Fork(h1, h2, (chars(h1) ++ chars(h2)).distinct, weight(h1) + weight(h2))
      combinedLeaf :: trees.tail.tail
    }
  }

  /**
    * This function will be called in the following way:
    *
    * until(singleton, combine)(trees)
    *
    * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
    * the two functions defined above.
    *
    * In such an invocation, `until` should call the two functions until the list of
    * code trees contains only one single tree, and then return that singleton list.
    *
    * Hint: before writing the implementation,
    *  - start by defining the parameter types such that the above example invocation
    * is valid. The parameter types of `until` should match the argument types of
    * the example invocation. Also define the return type of the `until` function.
    *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
    */
  def until(
             isSingleton: (List[CodeTree]) => Boolean,
             combiner: (List[CodeTree]) => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (isSingleton(trees)) trees
    else {
      val div = 2
//      val div = trees.size/2
      val sum = trees.foldLeft(0)((a, b) => a + weight(b))
      val middleW = sum /(trees.size-1)
      def f(t:CodeTree):Boolean = {
        weight(t) <= middleW
      }
      //      println(s"Size: ${trees.size}, Div: $div")
      val slided: List[List[CodeTree]] = if(3 < trees.size) trees.sliding(div, div).toList else List(trees)
//      val slided = trees.span(e => f(e))
//      val combined = combiner(slided._1) ++ combiner(slided._2)
      val combined = slided.flatMap(e => combiner(e))
//      var modified = combined.sortBy(e => weight(e))
//      println(s"M: ${trees.sliding(2,2).toList.map(e => combiner(e).head)}")
//      val combined = combiner(trees)
//      println(s"C: ${combined}")
//      println(s"S: ${modified}")
      until(isSingleton, combiner)(combined)
    }
  }

  def until0(
             isSingleton: (List[CodeTree]) => Boolean,
             combiner: (List[CodeTree]) => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (isSingleton(trees)) trees
    else until(isSingleton, combiner)(combiner(trees))
  }

  /**
    * This function creates a code tree which is optimal to encode the text `chars`.
    *
    * The parameter `chars` is an arbitrary text. This function extracts the character
    * frequencies from that text and creates a code tree based on them.
    */
  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }


  // Part 3: Decoding

  type Bit = Int

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def f(bs: List[Bit], t: CodeTree, cs: List[Char]): (List[Bit], List[Char]) = {
      if(chars(t).size == 1 || bs.isEmpty) {
//        println("== Found: " + chars(t).head)
        (bs, cs ++ List(chars(t).head))
      }
      else{
//        println("--"+bs.head)
//        println("--=="+t)
//        println("--=="+chars(t))
        if (bs.head == 0) f(bs.tail, left(t), cs)
        else f(bs.tail, right(t), cs)
      }
    }

    def f1(bs1: List[Bit], cs1: List[Char]): List[Char] = {
      val res = f(bs1, tree, cs1)
      val bs = res._1
      val cs = res._2
      if(bs.isEmpty) cs
      else f1(bs, cs)
    }

    if(bits.isEmpty) List()
    else f1(bits, List())
  }

  /**
    * A Huffman coding tree for the French language.
    * Generated from the data given at
    * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
    * What does the secret message say? Can you decode it?
    * For the decoding use the `frenchCode' Huffman tree defined above.
    **/
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
//    println(s"Tree:\n$tree\n\n")
//    println(s"TXT:\n${text.mkString}\n\n")
    def f(c: Char, t: CodeTree, bs: List[Bit]): List[Bit] = {
      if(chars(t).size == 1) {
//        println("^^Encoding: " + c)
//        println("^^==: " + bs)
        bs
      }
      else {
        def lt = left(t)
        def rt = right(t)
        if(chars(lt).contains(c)) f(c, lt, bs ++ List(0))
        else f(c, rt, bs ++ List(1))
      }
    }

    val res = text.flatMap(e => f(e, tree, List()))
//    println(s"RES:\n$res\n\n")
    res
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    val mm: Map[Char, List[Bit]] = table.toMap
//    println(mm)
    mm(char)
  }

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = {
    def f(t: CodeTree, ct: CodeTable): CodeTable = {
      if(chars(t).size == 1){
        ct
      }
      else{
        val ct1:CodeTable = chars(t).map( e => (e, if(chars(left(t)).contains(e)) List(0) else List(1)) )
        mergeCodeTables(
          mergeCodeTables(
            mergeCodeTables(ct, ct1),
            convert(left(t))),
          convert(right(t)))
      }
    }

    f(tree, List())
  }

  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    val ma: Map[Char, List[Bit]] = a.toMap
    val mb: Map[Char, List[Bit]] = b.toMap
    val mak = ma.keys.toList
    val mbk = mb.keys.toList
    val keys = (mak ++ mbk).distinct

    def f(ks: List[Char], acc: Map[Char, List[Bit]]): Map[Char, List[Bit]] = {
      if(ks.isEmpty) acc
      else{
        val k = ks.head
        val res = if(ma.keys.toList.contains(k)){
          if(mb.keys.toList.contains(k)){
            acc + (k -> (ma(k) ++ mb(k)))
          }
          else acc + (k -> ma(k))
        }
        else{
          acc + (k -> mb(k))
        }
        f(ks.tail, res)
      }
    }

    f(keys, Map()).toList
  }

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
//    println(new Throwable().printStackTrace(System.out))
    val table = convert(tree)
    text.flatMap(e => codeBits(table)(e))
  }

  def testList = {
    val zz = List(('a', 1), ('b', 2), ('s', 18), ('e', 18), ('o', 40), ('c', 2), ('z', 14))
    val ol = makeOrderedLeafList(zz)
    println(ol.size)
    val sum = ol.foldLeft(0)((a, b) => a + weight(b))
    val middleW = sum /2
    println(s"MW: $middleW")
    def f(t:CodeTree):Boolean = {
      weight(t) < middleW
    }
    println(ol.foldLeft((List(), 0))((acc, e) =>
      if (acc._2 < middleW)(acc._1, acc._2 + weight(e)) else (List(), 0)))
//    println(ol.span(e => f(e))._1)
//    println(ol.span(e => f(e))._2)
//    val div = ol.size / 2
//    println(ol.sliding(div, div).toList)
  }

  def main(args: Array[String]): Unit = {
    println("START")

//    testList
//    System.exit(0)

//    val str3 = "aabbaccxart7bac"
//    val str2 = "list should be ordered by"
//    val str3 = "list should be ordered by"
    val str3 = "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source."
//    val str3 = "zxc"
//    val list1 = times(str1.toList)
//    val list2 = times(str2.toList)
//    println(list1)
//    println(list2)
//    val ol1 = makeOrderedLeafList(list1)
//    val ol2 = makeOrderedLeafList(list2)
//
//    println("-------")
//    println(combine(makeOrderedLeafList(times("a".toList))))
//    println("-------")
//    println(ol1)
//    println(until(singleton, combine)(ol1).head)
//    val ct1 = createCodeTree(str1.toList)
//    println(ct1)
//    println("-------")
//
    val ct3 = createCodeTree(str3.toList)
    println(times(str3.toList))
    println(makeOrderedLeafList(times(str3.toList)))
    println(ct3)
    val bits3 = encode(ct3)(str3.toList)
    val bits3q = quickEncode(ct3)(str3.toList)
    println(bits3)
    println(bits3.size)
    println(bits3q)
    println(bits3q.size)
    println(decode(ct3, bits3))
    println(decode(ct3, bits3q))
//
//    val ct2 = createCodeTree(str2.toList)
//    val bits2_a = encode(ct2)("it should be border".toList)
//    println(bits2_a.size / 8)
//    println(decode(ct2, bits2_a))
//
//    println(decodedSecret)

//    val cta1 = List(('a', List(0,0)), ('b', List(0,1)), ('c', List(0,0,1)))
//    println(cta1)
//    println(codeBits(cta1)('b'))
  }
}
