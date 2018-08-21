package com.gm2211.template

object HandleBarPopulator {
  import com.gm2211.collections.MapExtensions._

  sealed trait ParserState
  case object NormalState extends ParserState
  case object FirstOpenBrace extends ParserState
  case object SecondOpenBrace extends ParserState
  case object ParsingPropertyName extends ParserState
  case object FirstClosingBrace extends ParserState
  case class InvalidSyntax(errorMessage: String) extends ParserState

  def populateTemplate(template: String, propertyValuesByName: Map[String, Any]): String = {

    sealed trait Token

    object Token {
      val ALLOWED_PROPERTY_NAME_CHARS: Set[Char] =
        ('A' to 'Z').union('a' to 'z').union('0' to '9').union(Seq('-', '_')).toSet

      def classify(char: Char): Token =
        if (char in ALLOWED_PROPERTY_NAME_CHARS) ValidPropertyNameToken else AnyOtherToken
    }

    case class CharToken(c: Char) extends Token
    case object AnyOtherToken extends Token
    case object ValidPropertyNameToken extends Token

    var curState: ParserState = NormalState

    val populatedTemplate = StringBuilder.newBuilder
    var candidatePropertyName = StringBuilder.newBuilder

    def noSecondBraceSoTreatCandidatePropertyNameAsPlaintext(char: Char) = {
      val valueWeThoughtCouldBeAProperty = candidatePropertyName.result()
      candidatePropertyName = StringBuilder.newBuilder
      populatedTemplate ++= s"{{$valueWeThoughtCouldBeAProperty}$char"
      NormalState
    }

    def treatCandidatePropertyNameAsPlaintext(char: Char, accumulatedSoFar: String) = {
      populatedTemplate ++= s"$accumulatedSoFar$char"
      NormalState
    }

    def propertyNameSuccessfullyParsed(ignored: Char) = {
      val propertyName = candidatePropertyName.result()
      candidatePropertyName = StringBuilder.newBuilder

      assert(propertyName isKeyOf propertyValuesByName, s"No value found for $propertyName")
      populatedTemplate ++= propertyValuesByName(propertyName).toString

      NormalState
    }

    def expectedClosingBraces(char: Char) = {
      InvalidSyntax(s"Illegal character $char. Expecting either '}' or any of ${Token.ALLOWED_PROPERTY_NAME_CHARS}")

    }

    def appendToPropertyName(char: Char) = {
      candidatePropertyName += char
      ParsingPropertyName

    }

    def appendToPopulatedTemplate(char: Char) = {
      populatedTemplate += char
      NormalState
    }

    val stateTransitions: Map[(ParserState, Token), Char => ParserState] = Map(
      (NormalState, CharToken('{')) -> { _: Char => FirstOpenBrace },
      (NormalState, ValidPropertyNameToken) -> appendToPopulatedTemplate,
      (NormalState, AnyOtherToken) -> appendToPopulatedTemplate,
      (FirstOpenBrace, CharToken('{')) -> { _: Char => SecondOpenBrace },
      (FirstOpenBrace, ValidPropertyNameToken) -> (treatCandidatePropertyNameAsPlaintext(_, "{")),
      (FirstOpenBrace, AnyOtherToken) -> (treatCandidatePropertyNameAsPlaintext(_, "{")),
      (SecondOpenBrace, CharToken('}')) -> { _: Char => InvalidSyntax("Braces closed too soon")},
      (SecondOpenBrace, ValidPropertyNameToken) -> appendToPropertyName,
      (SecondOpenBrace, AnyOtherToken) -> (treatCandidatePropertyNameAsPlaintext(_, "{{")),
      (ParsingPropertyName, CharToken('}')) -> { _: Char => FirstClosingBrace },
      (ParsingPropertyName, ValidPropertyNameToken) -> appendToPropertyName,
      (ParsingPropertyName, AnyOtherToken) -> expectedClosingBraces,
      (FirstClosingBrace, CharToken('}')) -> propertyNameSuccessfullyParsed,
      (FirstClosingBrace, ValidPropertyNameToken) -> noSecondBraceSoTreatCandidatePropertyNameAsPlaintext,
      (FirstClosingBrace, AnyOtherToken) -> noSecondBraceSoTreatCandidatePropertyNameAsPlaintext
    )

    for (char <- template) {
      val stateTransition = stateTransitions.getOrElse(
        (curState, CharToken(char)),
        stateTransitions(curState, Token.classify(char))
      )

      curState = stateTransition(char)
    }

    populatedTemplate.result()
  }
}
