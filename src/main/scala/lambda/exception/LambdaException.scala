package lambda.exception

object LambdaException {
  def create(msg: String): LambdaException = new LambdaException(msg)

  def create(msg: String, cause: Throwable) = new LambdaException(msg).initCause(cause)
}

class LambdaException(msg: String) extends RuntimeException(msg)