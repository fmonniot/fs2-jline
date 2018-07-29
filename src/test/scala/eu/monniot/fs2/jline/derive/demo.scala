package eu.monniot.fs2.jline.derive

// Until we have real test cases, we will use this simple example to be sure
// we aren't breaking the implicit derivation
object demo {


  // ADT for exploration

  sealed trait Cmd

  case class Ls(long: Boolean, human: Boolean) extends Cmd

  case object Add extends Cmd

  case object Rm extends Cmd

  case class Thing()

  // Invocation

  val mk1 = MkCoOpts[Thing]
  val mk2 = MkCoOpts[Ls]

  val cmd = command[Ls]
  val cmds = commands[Cmd]

}
