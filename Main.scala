// This program is writen by Larry Tieken for class csc 315. 
// I have used geeksforgeeks for inspiration for this program
// The website I used is https://www.geeksforgeeks.org/playfair-cipher-with-examples/

import scala.io.Source
import scala.util.control.Breaks._
import java.io._

object PlayFairCipher{

  // This is a function I made for searching through a 2 demensianal array that is provided by
  // the scala library called array.ofDim. In this function I also used the scala.util.control.breaks._
  // library for breaking the for loops when it find the match. 
  def arrySearcher(arg: Array[Array[String]], searchChar: String): (Int,Int) = {
    var index = (0,0)

    // This is how you tell scala you want the loop to be breakable
    breakable {
      for(x<-0 to 4){
        for(y<-0 to 4){
          if(arg(x)(y) == searchChar){
            index = (x,y)

            // Here is whereI break the for loops 
            break
          } //if 
        } // for
      } // for
    } // breakable
    return(index)
  }// def arraySearcher


  def main(args: Array[String]): Unit = {

  // This is where I use the Print Writer library to make a text file to write in 
    val pw = new PrintWriter(new File("encipher.txt"))

    // This line till 44 is where I open the text file to read it 
    val filename = """\anasofiaaguiarazambuja\Downloads\encipher.txt"""

    val fileContents = Source.fromFile(filename).getLines.mkString

    var sampleString = fileContents


    var charIndexOne = (0,0)

    var charIndexTwo = (0,0)

    var charOne = " "

    var charTwo = " "

    // This is where I use the special Scala library to make a 2darray 
    // then in the lines until 83 is where I define all of the contents of the array 
    val myMultiArr = Array.ofDim[String](5,5)

    myMultiArr(0)(0) = "a"              
    myMultiArr(0)(1) = "f"
    myMultiArr(0)(2) = "l"
    myMultiArr(0)(3) = "q"
    myMultiArr(0)(4) = "v"
    myMultiArr(1)(0) = "b"
    myMultiArr(1)(1) = "g"
    myMultiArr(1)(2) = "m"
    myMultiArr(1)(3) = "r"
    myMultiArr(1)(4) = "w"
    myMultiArr(2)(0) = "c"
    myMultiArr(2)(1) = "h"
    myMultiArr(2)(2) = "n"
    myMultiArr(2)(3) = "s"
    myMultiArr(2)(4) = "x"
    myMultiArr(3)(0) = "d"
    myMultiArr(3)(1) = "i"
    myMultiArr(3)(2) = "o"
    myMultiArr(3)(3) = "t"
    myMultiArr(3)(4) = "y"
    myMultiArr(4)(0) = "e"
    myMultiArr(4)(1) = "k"
    myMultiArr(4)(2) = "p"
    myMultiArr(4)(3) = "u"
    myMultiArr(4)(4) = "z"

    
    // this is where I add onto the string provided if it is a odd number of characters
    if(sampleString.length % 2 == 1 )
      sampleString = sampleString + "x"
    
    // this is where i add x's if there are repeated characters 
    for(i<-0 to sampleString.size - 1){ 

      if(i < sampleString.size - 1){


        if(sampleString(i) == 'j')
          sampleString = sampleString.updated(i,'i')
        

        if(sampleString(i) == sampleString(i+1) && sampleString(i) != 'x')
          sampleString = sampleString.updated(i + 1, 'x')

      }
    } // for
  
    println(sampleString)

    // this is where I make the array for the strings that hold two of the characters in each index for the breakdown of the message. 
    var arrayOf2Strings : Array[String] = new Array[String](sampleString.length / 2)

    // this is where I parse the message and load it into the array of 2 strings 
    for(i<-0 to arrayOf2Strings.length - 1){
      arrayOf2Strings(i) = sampleString.take(2)
      sampleString = sampleString.drop(2)
    }

    // this is a test printer for the messages to test accuracy 
    for(i<-0 to arrayOf2Strings.length - 1)
      println(arrayOf2Strings(i))
  

    //This is the for loop that finds the index of each of the characters then
    // decrepts them in accordance to there place in the 2d array 
    // the first if is where it checks if it is in the second collumn 
    // the else if is where it checks if it is in the same row
    // the last else is where I know that they are going to be in a box 
    // so it changes the letters according to there place in the matrix 
    for(i<-0 to arrayOf2Strings.length - 1){
      // Here is where I split the characters and load them in there own variable for the function to find the index 
      charOne = arrayOf2Strings(i).dropRight(1)
      charTwo = arrayOf2Strings(i).drop(1)

      // this is where I call arrySearcher to find the place of the character in the 2d array 
      charIndexOne = arrySearcher(myMultiArr, charOne)
      charIndexTwo = arrySearcher(myMultiArr, charTwo)

      // here is where it checks if it is in the same collumn 
      if(charIndexOne._1 == charIndexTwo._1){

        // these next 2 ifs makes sure it isnt going to go out of bounds by hiting 5 so it round robins to 1 
        if(charIndexOne._2 == 4){
          charOne = myMultiArr(charIndexOne._1)(1)
        }else{
          charOne = myMultiArr(charIndexOne._1)(charIndexOne._2 + 1)
        }
        
        if(charIndexTwo._2 == 4){
          charTwo = myMultiArr(charIndexTwo._1)(1)
        }else{
          charTwo = myMultiArr(charIndexTwo._1)(charIndexTwo._2 + 1)
        }


        // this is where i check if there in the same row with the virtualy same ifs as the last if 
      }else if(charIndexOne._2 == charIndexTwo._2){

        if(charIndexOne._1 == 4){
          charOne = myMultiArr(1)(charIndexOne._2)
        }else{
          charOne = myMultiArr( charIndexOne._1 + 1)(charIndexOne._2)
        }

        if(charIndexTwo._1 == 4){
          charTwo = myMultiArr(1)(charIndexTwo._2)
        }else{
          charTwo = myMultiArr( charIndexTwo._1 + 1)(charIndexTwo._2)
        }
        // this is where I set the characters to the box format if it fails the last two ifs
        }else{
          charOne = myMultiArr(charIndexOne._1)(charIndexTwo._2)
          charTwo = myMultiArr(charIndexTwo._1)(charIndexOne._2)
      }

      // this is where is loads the array of 2 strings with the encripted code 
      arrayOf2Strings(i) = charOne+charTwo
      // this is a test print 
      println(arrayOf2Strings(i))
      // here is where I write to the text file 
      pw.write(arrayOf2Strings(i))
    } // for
    // this is where the file gets closed 
    pw.close()

  }// def main

} // object PlayFairCipher