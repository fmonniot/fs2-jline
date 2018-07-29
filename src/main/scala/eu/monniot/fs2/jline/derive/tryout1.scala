package eu.monniot.fs2.jline.derive

import cats.implicits._


/* Testing live zone :)
 */
object tryout1 {

  case class Thing()

  val mk1 = MkCoOpts[Thing]
  val mk2 = MkCoOpts[Ls]


  val cmd = command[Ls]
  val cmds = commands[Cmd]

}
