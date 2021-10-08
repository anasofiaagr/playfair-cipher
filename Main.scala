import scala.language.postfixOps

// implements the Playfair cipher
object Playfair {
  val alphabet = "abcdefghiklmnopqrstuvwxyz"
  val Header = """([a-zA-Z]+)\s+([a-zA-Z]+[\s[a-zA-Z]]*)""".r

  def main(args: Array[String]): Unit = {
    import java.io._
    import scala.io.Source
    import javax.swing.JFileChooser

    val fileChooser =
      new JFileChooser() //displays a JFileChooser, selectes file,
    // and runs the Playfair cipher with it
    fileChooser.showOpenDialog(null)

    val lines = Source.fromFile(fileChooser.getSelectedFile).getLines().toList

    try {
      val Header(direction, keyword) =
        (lines head).toLowerCase.replaceAll("j", "i") // to replace j for i

      val cipher =
        (keyword + alphabet)
          .replaceAll("""\s+""", "")
          .toList
          .distinct // list with no duplicates
      val message = (lines tail)
      .reduceLeft(_ + _)
      .toLowerCase
      .replaceAll("j", "i")
      .replaceAll("""[^a-zA-Z]""", "")

      // function to get the coordinates of a character in the cipher
      val cipherCoordinates = (x: Char) => {
        Tuple2((cipher.indexOf(x)) / 5, cipher.indexOf(x) % 5)
      }

      // function to get a character in the cipher by its coordinates
      val charAt = (row: Int, col: Int) => {
        val index = {
          if (col < 0) {
            (5 * row) + col + 5
          } else if (col >= 5) {
            (5 * row) + col - 5
          } else {
            (5 * row) + col
          }
        }
        if (index < 0) {
          cipher(index + cipher.length).toString
        } else if (index >= cipher.length) {
          cipher(index - cipher.length).toString
        } else {
          cipher(index).toString
        }
      }

      direction match { // found interesting pattern matching
        // "A match expression has a value, the match keyword, and at least one case clause"
        case "encipher" =>
          println(
            formatMessage(
              encipherMessage(cipherCoordinates, charAt, message),
              1
            )
          )
        case _ => println("Error!")
      }
    } catch {
      case ex: MatchError =>
        println(
          """Error:  First line of file must be 'encipher <keyword>' or 'decipher <keyword>'"""
        )
    }
  }

  // formats the output message for display as specified
  def formatMessage(message: String, count: Int): String = { // message, the message to format and count, the
    // number of characters seen so far
    if (message.length > 0) {
      if (count % 50 == 0) {
        message.charAt(0) + System
          .getProperty( // getproperty returns a string containing the value of the property
            "line.separator"
          ) + formatMessage(message.substring(1), count + 1)
      } else if (count % 5 == 0) {
        message.charAt(0) + " " + formatMessage(message.substring(1), count + 1)
      } else {
        message.charAt(0) + formatMessage(message.substring(1), count + 1)
      }
    } else {
      return message // returns the message, in blocks of 5 characters separated by spaces, with
      // newlines every 50 characters
    }
  }

  def encipherMessage( // enciphers the message
      cipherCoordinates: Char => Tuple2[
        Int,
        Int
      ], // function to get the row and column of a character in the cipher
      charAt: (
          Int,
          Int
      ) => String, // function to get the cipher character at the given row and column
      message: String // enciphered message
  ): String = {
    if (message.length > 0) {
      if (message.length == 1 || message.charAt(0) == message.charAt(1)) {
        val (firstRow, firstCol) = cipherCoordinates(message.charAt(0))
        val (secondRow, secondCol) = cipherCoordinates('x')

        if (firstRow == secondRow) {
          charAt(firstRow, firstCol + 1) + charAt(secondRow, secondCol + 1) +
            encipherMessage(cipherCoordinates, charAt, message.substring(1))
        } else if (firstCol == secondCol) {
          charAt(firstRow + 1, firstCol) + charAt(secondRow + 1, secondCol) +
            encipherMessage(cipherCoordinates, charAt, message.substring(1))
        } else {
          charAt(firstRow, secondCol) + charAt(secondRow, firstCol) +
            encipherMessage(cipherCoordinates, charAt, message.substring(1))
        }
      } else {
        val (firstRow, firstCol) = cipherCoordinates(message.charAt(0))
        val (secondRow, secondCol) = cipherCoordinates(message.charAt(1))

        if (firstRow == secondRow) {
          charAt(firstRow, firstCol + 1) + charAt(secondRow, secondCol + 1) +
            encipherMessage(cipherCoordinates, charAt, message.substring(2))
        } else if (firstCol == secondCol) {
          charAt(firstRow + 1, firstCol) + charAt(secondRow + 1, secondCol) +
            encipherMessage(cipherCoordinates, charAt, message.substring(2))
        } else {
          charAt(firstRow, secondCol) + charAt(secondRow, firstCol) +
            encipherMessage(cipherCoordinates, charAt, message.substring(2))
        }
      }
    } else {
      return message
    }
  }
} // most of the code is from github source yet removed decipher parts.
