/* Generated By:JavaCC: Do not edit this line. ParserConstants.java */
package parser;


/**
 * Token literal values and constants.
 * Generated by org.javacc.parser.OtherFilesGen#start()
 */
public interface ParserConstants {

  /** End of File. */
  int EOF = 0;
  /** RegularExpression Id. */
  int EOL = 4;
  /** RegularExpression Id. */
  int PLUS = 5;
  /** RegularExpression Id. */
  int MINUS = 6;
  /** RegularExpression Id. */
  int MULT = 7;
  /** RegularExpression Id. */
  int DIV = 8;
  /** RegularExpression Id. */
  int LPAR = 9;
  /** RegularExpression Id. */
  int RPAR = 10;
  /** RegularExpression Id. */
  int INT = 11;
  /** RegularExpression Id. */
  int DIGIT = 12;

  /** Lexical state. */
  int DEFAULT = 0;

  /** Literal token values. */
  String[] tokenImage = {
    "<EOF>",
    "\" \"",
    "\"\\r\"",
    "\"\\t\"",
    "\"\\n\"",
    "\"+\"",
    "\"-\"",
    "\"*\"",
    "\"/\"",
    "\"(\"",
    "\")\"",
    "<INT>",
    "<DIGIT>",
  };

}
