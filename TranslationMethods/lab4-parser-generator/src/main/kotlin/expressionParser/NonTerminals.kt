package expressionParser

import generator.runtime.NonTerminal
import java.math.BigDecimal

public data class Expr(
  public var res: BigDecimal? = null,
) : NonTerminal()

public data class Expr_(
  public var res: BigDecimal? = null,
) : NonTerminal()

public data class Term(
  public var res: BigDecimal? = null,
) : NonTerminal()