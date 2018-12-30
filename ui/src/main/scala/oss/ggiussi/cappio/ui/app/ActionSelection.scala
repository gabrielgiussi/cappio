package oss.ggiussi.cappio.ui.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import oss.ggiussi.cappio.core.LinkProtocol.Message
import oss.ggiussi.cappio.{InstanceID, ProcessID}
import oss.ggiussi.cappio.core.{Action, LinkProtocol}
import oss.ggiussi.cappio.impl.bcast.BrokenBcastProtocol
import oss.ggiussi.cappio.impl.faildet.PerfectFailureDetectorProtocol.Crashed
import oss.ggiussi.cappio.impl.processes.ProcessProtocol.Crash

object ActionSelection {

  type ToCallback[F] = ToCallbackTo[F, Unit]

  type ToCallbackTo[F, T] = F => CallbackTo[T]

  object ToCallback {
    def apply[F](f: F => Unit): ToCallback[F] = ToCallbackTo(f)

  }

  object ToCallbackTo {
    def apply[F, T](f: F => T): ToCallbackTo[F, T] = (i: F) => CallbackTo(f(i))

  }

  type ActionSelection = ToCallback[Action]
  type MessageSelection = ToCallback[Message]

  case class MessageSelectorProps(callback: MessageSelection)

  val MessageSelector = ScalaComponent.builder[MessageSelectorProps]("Message")
    .render_P { case MessageSelectorProps(callback) =>
      def c(e: ReactEventFromInput) = CallbackTo {
        e.preventDefault()
        e.extract(_.target.value)(value => Message(value))
      } >>= callback

      <.input(
        ^.onChange ==> c
      )
    }.build

  val CrashSelector = ScalaComponent.builder[(InstanceID, Set[ProcessID],ActionSelection)]("CrashSelector")
    .initialState[Option[ProcessID]](None)
    .renderPS { case (x,(instance, processes,callback),selected) =>
      <.div(
        ProcessSelector(processes,(e: Option[ProcessID]) => x.setState(e)),
        <.button(
          ^.onClick -->? (for {
            s <- selected
          } yield callback(Crash(s, instance))),
          "Next"
        )
      )
    }.build

  case class State(from: Option[ProcessID], to: Option[ProcessID], msg: Option[Message])

  val ProcessSelector = ScalaComponent.builder[(Set[ProcessID], ToCallback[Option[ProcessID]])]("ProcessSelector")
    .initialState[Option[ProcessID]](None)
    .renderPS { case (x, (processes, callback), selected) =>
      def onChange(e: ReactEventFromInput): Callback = e.extract(_.target.value)(v => {
        val s = Option(v).filterNot(_ == "-").map(_.toInt)
        x.setState(s) >> CallbackTo(s) >>= callback
      })

      <.select(
        ^.value := selected.map(_.toString).getOrElse("-"),
        ^.onChange ==> onChange,
        <.option(
          ^.value := "-",
          "-"
        ),
        processes.toVdomArray(p => <.option(
          ^.key := p,
          ^.value := p,
          p
        )),

      )
    }.build


  val Send = ScalaComponent.builder[(InstanceID, ActionSelection)]("SendInput")
    .initialState[State](State(None, None, None))
    .renderPS { case (x, (instance, callback), s) =>
      <.div(
        ProcessSelector((
          Set(1, 2),
          (e: Option[ProcessID]) => x.modState(_.copy(from = e))
        )),
        ProcessSelector((
          Set(1, 2),
          (e: Option[ProcessID]) => x.modState(_.copy(to = e))
        )),
        MessageSelector(
          MessageSelectorProps((m: Message) => x.modState(_.copy(msg = Some(m))))
        ),
        <.button(
          ^.onClick -->? (for {
            _f <- s.from
            _t <- s.to
            _m <- s.msg
          } yield callback(LinkProtocol.send(_f, _t, instance, _m))),
          "Next"
        )
      )
    }
    .build

  case class BcastState(from: Option[ProcessID], msg: Option[Message])

  case class BcastProps(instance: InstanceID, callback: ActionSelection, processes: Set[ProcessID])

  val Bcast = ScalaComponent.builder[BcastProps]("Bcast")
    .initialState(BcastState(None, None))
    .renderPS { case (x, BcastProps(instance, callback, processes), BcastState(from, msg)) =>
      <.div(
        ProcessSelector((
          processes,
          (e: Option[ProcessID]) => x.modState(_.copy(from = e))
        )),
        MessageSelector(
          MessageSelectorProps((m: Message) => x.modState(_.copy(msg = Some(m))))
        ),
        <.button(
          ^.onClick -->? (for {
            _f <- from
            _m <- msg
          } yield callback(BrokenBcastProtocol.bcast(_f, instance, _m))),
          "Next"
        )
      )
    }.build


  val ActionList = ScalaComponent.builder[(Set[Action], ActionSelection)]("ActionList")
    .render_P { case (actions, callback) =>
      <.div(
        "Actions",
        <.ul(
          actions.zipWithIndex.toVdomArray { case (action, index) =>
            <.li(
              ^.onClick --> callback(action),
              action.toString,
              ^.key := index
            )
          }
        )
      )
    }.build

}
