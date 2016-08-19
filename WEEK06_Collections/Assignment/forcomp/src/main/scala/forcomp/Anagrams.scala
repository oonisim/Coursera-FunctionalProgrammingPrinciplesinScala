package forcomp

import util.control.Breaks._
object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary
  //val dictionary: List[Word] = List("I", "love", "sushi", "hamachi", "like")

  /**
   * Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    // "abbccc".groupBy(c => c) creates Map((a -> a), (b -> bb), (c -> ccc)).  
    (for ((char, str) <- w.toLowerCase().groupBy(c => c) if (char.isLetter)) yield (char, str.length)).toList.sorted
  }

  /** Converts a sentence into its character occurrence list. */
  //def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.flatten.toString)
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.flatten.mkString)

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    //--------------------------------------------------------------------------------
    // If there is no matching key/occurrences for the map, give Nil.
    //--------------------------------------------------------------------------------
    dictionary.distinct.groupBy(word => wordOccurrences(word)) withDefaultValue List[Word]()
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    //--------------------------------------------------------------------------------
    // [Logic]
    // All combinations(X, Y, Z) = combination of X included + Combination of X excluded.
    //--------------------------------------------------------------------------------
    //println("combinations in. occurances = " + occurrences)
    if (occurrences.isEmpty) List(List[(Char, Int)]()) // NOT List[Occurrences]()
    else {
      val (c, n) = occurrences.head
      val excluded = combinations(occurrences.tail)
      val included = for {
        i <- (1 to n)
        _occurance <- excluded
      } yield {
        //println("_occurance = " + _occurance + " generating " + (c -> i) :: _occurance)
        (c -> i) :: _occurance
      }
      (included.toList ::: excluded)
    }
  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  //--------------------------------------------------------------------------------
  // Subtract only legal when y is sub set of x, which is x frequency >= y frequency.
  // This condition is assured because:
  // 1. If the sentence has the occurrences such as ((a, 5), (b, 3), (c, 1)), then 
  //    a potential anagram sentence is decomposed as (word + sub sentence). 
  // 2. Subtract is used to get remaining available occurrences for the sub sentence.
  //    For example if word occurrences is (a, 2), it is always a sub set of the
  //    sentence occurrences ((a, 5), (b, 3), (c, 1)).
  // 
  // When no element left such as subtract(('a', 1), ('a', 1), then it is legal.
  // In this case, Nil is the result.
  //--------------------------------------------------------------------------------
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    //--------------------------------------------------------------------------------
    // ymap is a Map of (key, frequency) to get the frequency for the key.
    // If there is no key match, then give 0, then the frequency of x will remain.
    //--------------------------------------------------------------------------------
    val ymap = y.toMap withDefaultValue 0

    //--------------------------------------------------------------------------------
    // Generate a new list of element List(key, xf-yf) by subtracting frequency of y 
    // element from that of x element. The return type is List so that if the frequencies
    // are the same, it can return List().
    // Need to handle the cases of:
    // 1. There is no key in y. 
    //    When x = (a, 2), (b, 1) and y = (a, 2), there is no 'b' in ymap. 
    //    Then the x element itself so that the caller keep appending (b, 1) ...
    // 2. (xf - yf) is zero.
    //    Need to remove the 'element' from the occurrences of the caller.
    //    Hence, Nil so that appending it is same with removing it.
    // 3. (xf - yf) is minus.
    //     Subtracting is illegal, hence RETURN None to stop the subtract.
    //--------------------------------------------------------------------------------
    def newelement(element: (Char, Int)): List[(Char, Int)] = {
      val (key, xf) = element
      val yf = ymap(key)

      require(xf >= yf, "x frequency must be >= that of y")
      if (xf == yf) Nil
      else if (yf == 0) List(element)
      else List((key, xf - yf))
    }
    //--------------------------------------------------------------------------------
    // Build a new x by accumulating new x elements.
    //--------------------------------------------------------------------------------
    def build(accumulator: Occurrences, element: (Char, Int)): Occurrences = {
      accumulator ++ newelement(element)
    }
    x.foldLeft(List[(Char, Int)]())((_accumulator, element) => build(_accumulator, element))
  }

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence.isEmpty) List(sentence)
    create(sentenceOccurrences(sentence), 0) match {
      case None => Nil
      case Some(sentences) => sentences
    }
  }
  //--------------------------------------------------------------------------------
  // Create anagram sentences for all the remaining available occurrences.
  //--------------------------------------------------------------------------------
  def create(remaining: Occurrences, level: Int): Option[List[Sentence]] = {
    if (remaining.isEmpty) Some(List(List()))
    else {
      val anagrams = for {
        //--------------------------------------------------------------------------------
        // Create possible occurrences combinations.
        // For each pattern in the combinations, get meaningful words from the dictionary.
        // For each word in words, create anagram sentences, including the word itself. 
        //--------------------------------------------------------------------------------
        occurrence <- combinations(remaining)
        word <- dictionaryByOccurrences(occurrence)
      } yield {
        create(subtract(remaining, occurrence), level + 1) match {
          case None => {
            //--------------------------------------------------------------------------------
            // This keep adding Nil into the anagrams, hence result could be like below.
            // Need to find out a way not to collect Nil in this for comprehension.
            // In the meantime, apply filter(!isEmpty) on the result.
            //
            // List(
            //   List("I", "love", "sushi"),
            //   List("sushi", "I", "love"),
            //   List(),
            //   List("love", "sushi", "I"),
            //   List()
            //   List(,
            // }
            //--------------------------------------------------------------------------------
            Nil
          }
          case Some(childs) => {
            //--------------------------------------------------------------------------------
            // List( <----- For accumulatees List[Sentence] into another list.
            //   List(
            //     List("I", "love", "sushi"),
            //     List("sushi", "I", "love")
            //   ),
            //   List (
            //     List("love", "sushi", "I")
            //   )
            // )
            // 
            // Need to flatten the result into a single List[Sentence] the end.
            // List(
            //     List("I", "love", "sushi"),
            //     List("sushi", "I", "love")
            //     List("love", "sushi", "I")
            //)
            //--------------------------------------------------------------------------------
            def concat(accumulator: List[Sentence], child: Sentence): List[Sentence] = {
              //--------------------------------------------------------------------------------
              // Create a parent sentence by (word + child sentence) and accumulate.
              //--------------------------------------------------------------------------------
              accumulator :+ (word :: child)
            }
            childs.foldLeft(List[Sentence]())(concat)
          }
        }
      }.filter(x => !x.isEmpty)
      //--------------------------------------------------------------------------------
      // If no sentences could be made for the available occurrences (remaining) > 0,
      // then, this remaining occurrences is invalid.
      //--------------------------------------------------------------------------------
      if (anagrams.isEmpty) None
      else {
        // for (i <- 0 until level) print("\t")
        //println("generated count = %d, contents = %s".format(result.length, result))
        Some(anagrams.flatten)
      }
    }
  }
}

import forcomp.Anagrams._
import com.sun.org.apache.xerces.internal.impl.xs.models.XSDFACM.Occurence

object AnagramsTest extends App {
  val fish = List("uni", "tako", "saba", "abas")
  //println(fish.distinct.groupBy(word => wordOccurrences(word)))
  //println(wordAnagrams("sacal"))
  //println(combinations(List(('a', 2), ('b', 2))))
  //println(subtract(List(('a', 3), ('b', 2), ('c', 1)), List(('a', 2), ('b', 2))))
  //println(subtract(List(('a', 3), ('b', 2)), List(('a', 3), ('b', 2))))
  //println(wordOccurrences("love"))
  //println(sentenceOccurrences(List("love", "h"))) 
  //val dictionary: List[Word] = List("I", "love", "sushi", "hamachi", "like")
  // println("dictionary -= " + dictionaryByOccurrences)
  println(sentenceAnagrams(List("i", "lovesushi")))
  List(Nil).foldLeft(List("abc"))((_accumulator, _sentence) => _accumulator ::: _sentence)
}
