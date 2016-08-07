//================================================================================
// Functional Programming Principles in Scala - Getting Started + Functions & Evaluation 
// URL: https://www.coursera.org/learn/progfun1/programming/Ey6Jf
//================================================================================
package recfun
object Main {
    var maps = scala.collection.mutable.Map[Int, Int]();
    def printmaps():Unit = {
        println(maps);
        maps = scala.collection.mutable.Map[Int, Int]();
    }

    def main(args: Array[String]) {
        println("Pascal's Triangle")
        for (row <- 0 to 10) {
          for (col <- 0 to row)
            print(pascal(col, row) + " ")
          println()
        }
        /*
        val l:List[Char] = "())(".toList;
        println(balance(l));
        println(countChange(9, List(5, 3, 1)));
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
    //================================================================================
    def balance(chars: List[Char]): Boolean = {
        def findRight(ci: Iterator[Char]): Boolean = {
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
	/* 
         * !!! The purpose of functional program and recursion is NOT to use loop which is basically jump!
         * Instead of while/iterator, use list.head and list.tail. Divide into tree leaves.
         */
        while(ci.hasNext){	
            var c = ci.next();
            if(c == ')'){
                return(false);
            } else if(c == '('){
                if(!findRight(ci)) return(false);
            }
        }
        return(true);
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
            return(0); 
        }
        if(money == 0){
            //--------------------------------------------------------------------------------
            // There is one combination identified at the caller function which is without using 
            // the coins.head here.
            //--------------------------------------------------------------------------------
            //printmaps();
            return(1);
        }  
        if(coins.isEmpty){
            //--------------------------------------------------------------------------------
            // All denominations are exhuasted. No coin is left available to count combination.
            // If the caller function has come up with combination with remaining money is 0,
            // then the caller function has identified a combinaiton using all the denominations.
            // If not, the caller fucntion has asked to find out a combination that cannot exit.
            // Hence return 0.
            //--------------------------------------------------------------------------------
            return(0);
        }

        val denomi:Int = coins.head;
        var ncoins:Int = 0;
        var count:Int = 0;

	/* 
         * !!! The purpose of functional program and recursion is NOT to use loop which is basically jump!
         * Instead of deducting (denomi * coins), call countChange(money - coins.head, coins).
         */
        while( (denomi * ncoins) <= money ){
            // Use coins (0, 1, ...) and search for combination without the coin.
            maps.update(denomi, ncoins);
            count += countChange(money - (denomi * ncoins), coins.tail);
            ncoins += 1;
        }
        return(count);
    }
}