/*================================================================================
[Subject]
Coursera Functional Programming Principles in Scala
Week 01 Programming Assignment: Recursion

[URL]
https://www.coursera.org/learn/progfun1/programming/Ey6Jf

[Result]
Your overall score for this assignment is 10.00 out of 10.00
Our automated style checker tool could not find any issues with your code. 
You obtained the maximal style score of 2.00.

[History]
Version 1.0	07 AUG 2016
================================================================================*/
package recfun
object Main {
    /*
    var maps = scala.collection.mutable.Map[Int, Int]();
    def printmaps():Unit = {
        println(maps);
        maps = scala.collection.mutable.Map[Int, Int]();
    }
    */
    def main(args: Array[String]) {
        println("Pascal's Triangle")
        for (row <- 0 to 10) {
          for (col <- 0 to row)
            print(pascal(col, row) + " ")
          println()
        }
        /*
        println(countChange(9, List(5, 3, 1)));
        val l:List[Char] = "I am (hoge()((hogehoge)tako(a)ika)".toList;
        println(balance(l));
        */

   }
   
    //================================================================================
    // [Exercise 1]
    // Implementing the pascal triangle function which takes a column c and a row r, 
    // counting from 0 and returns the number at that spot in the triangle. 
    // For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
    //================================================================================
    def pascal(c: Int, r: Int): Int = {
        if(c == 0 || c == r){
            1;
        } else {
            pascal(c-1, r-1) + pascal(c, r-1);
        }
    }  

    //================================================================================
    // [Exercise 2]
    // Write a recursive function which verifies the balancing of parentheses in string, 
    // which we represent as a List[Char] not a String. For example, the function should 
    // return true for the following strings:
    // * (if (zero? x) max (/ 1 x))
    // * I told him (that it’s not (yet) done). (But he wasn’t listening)
    // The function should return false for the following strings:
    // * :-) 
    // * ())(   
    // [Note]
    // Aling with the functional programming principles:
    // * Tail recursion instead of loop.
    // * Use evaluation not return.
    // * Immutable.
    //================================================================================
    def balance(chars: List[Char]): Boolean = {
	/*
    	def findRight(ci: Iterator[Chjavascript:void(0)ar]): Boolean = {
            while(ci.hasNext){
                var c = ci.next();
                if (c == '('){
                    if(!findRight(ci)) return(false);
                } else if(c == ')'){
                    return(true);
                }
            }
            return(false);
        }

        val ci:Iterator[Char] = chars.iterator;
        while(ci.hasNext){	
            var c = ci.next();
            if(c == ')'){
                return(false);
            } else if(c == '('){
                if(!findRight(ci)) return(false);
            }
        }
        return(true);
	*/

        //--------------------------------------------------------------------------------
        // Val level keeps tracks of the nested parentheses level.
        //--------------------------------------------------------------------------------
        val level:Int = 0;
        def find(chars: List[Char], level:Int):Boolean = {
            if(chars.isEmpty) {
                //--------------------------------------------------------------------------------
                // End of the input. 
                // If find() is in the middle of searching for ')' to be paird with '(', then 
                // find the match failed -> unbalanced.
                //--------------------------------------------------------------------------------
                if (level > 0) false;
                else true;
            } else {
                chars.head match {
                    case ')' => {
                        //--------------------------------------------------------------------------------
                        // When right parenthsis is found without having encountered left (level <= 0)
                        // then the input included unpaired right parenthsis -> False.
                        // Otherwise, it is the right one for the left for which the find() is invoked.
                        // Go one nest level up and continue looking for ')' for the left of previous find().
                        //--------------------------------------------------------------------------------
                        if(level > 0) find(chars.tail, level -1);
                        else false;
                    }
                    case '(' => {
                        //--------------------------------------------------------------------------------
                        // When 'left parenthsis'(' is found, increase the nest level and start a new find() 
                        // for ')' to be paried with.
                        //--------------------------------------------------------------------------------
                        find(chars.tail, level + 1);
                    }
                    case default => {
                        //--------------------------------------------------------------------------------
                        // Keep looking for the ')' for the current find().
                        //--------------------------------------------------------------------------------
                        find(chars.tail, level);
                    }
                }
            }
        }
        find(chars, level);
    }
    
    //================================================================================
    // [Exercise 3]
    // Write a recursive function that counts how many different ways you can make to 
    // change for an amount, given a list of coin denominations. 
    // For example, there are 3 ways for 4 if with coins 1 and 2: 1+1+1+1, 1+1+2, 2+2.
    //================================================================================
    def countChange(money:Int,coins:List[Int]):Int = {
        if(money < 0){
            //--------------------------------------------------------------------------------
            // There is no combination available.
            //--------------------------------------------------------------------------------
            0; 
        } else if(money == 0){
            //--------------------------------------------------------------------------------
            // There is one combination identified at the caller function which is without using 
            // the coins.head here.
            //--------------------------------------------------------------------------------
            //printmaps();
            1;
        } else if(coins.isEmpty){
            //--------------------------------------------------------------------------------
            // All denominations are exhuasted. No coin is left available to count combination.
            // If the caller function has come up with combination with remaining money is 0,
            // then the caller function has identified a combinaiton using all the denominations.
            // If not, the caller fucntion has asked to find out a combination that cannot exit.
            // Hence return 0.
            //--------------------------------------------------------------------------------
            0;
        } else {
        	/* 
             * !!! The purpose of functional program and recursion is NOT to use loop which is basically jump!
             * Instead of deducting (denomi * coins), call countChange(money - coins.head, coins).
             */
            /*
            while( (denomi * ncoins) <= money ){
                // Use coins (0, 1, ...) and search for combination without the coin.
                //maps.update(denomi, ncoins);
                count += countChange(money - (denomi * ncoins), coins.tail);
                ncoins += 1;
            }
            return(count);
            */
            // Use the head coin (denomination is coins.head) and not use it.
            countChange(money - coins.head, coins) + countChange(money, coins.tail);
        }
    }
}
