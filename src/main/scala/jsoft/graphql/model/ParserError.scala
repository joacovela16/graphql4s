package jsoft.graphql.model

import fastparse.Parsed

case class ParserError(error: Parsed.Failure) extends Exception(error.msg.replace("\"", "'"))
