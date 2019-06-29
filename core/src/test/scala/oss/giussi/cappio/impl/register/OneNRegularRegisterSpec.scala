package oss.giussi.cappio.impl.register

import java.util.UUID

import oss.giussi.cappio.Messages.{LocalRequest, LocalStep, PublicRequest}
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.BebBcast
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.register.OneNRegularRegister.{ONACK, ONREAD, ONRRRead, ONRRReadReturn, ONRRState, ONRRStateI, ONRRWrite, ONRRWriteReturn, ONVALUE, ONWRITE}
import oss.giussi.cappio.{CappIOSpec, NextState, Packet, ProcessId}

class OneNRegularRegisterSpec extends CappIOSpec {


  val p0 = ProcessId(0)
  val p1 = ProcessId(1)
  val p2 = ProcessId(2)
  val all = Set(p0,p1,p2)
  val timeout = 100
  val N = all.size
  val ps = all.map(id => id.id -> OneNRegularRegister.init[Int](id, N, timeout, all)).toMap

  "A" should {
    "B" in {
      ps(0).request(ONRRRead).send.map { case FLLSend(Packet(_, ONREAD(1), ProcessId(0), to, OneNRegularRegister.BEB)) => to } shouldBe all
    }

    "C" in {
      ps(0).request(ONRRWrite(1)).send.map { case FLLSend(Packet(_,ONWRITE(1,1),ProcessId(0),to,OneNRegularRegister.BEB)) => to } shouldBe all
    }

    "D" in {
      ps(0).request(ONRRWrite(1))
        // aca estoy salteando el bcast(WRITE)
        .deliver(Packet(p1,p0,ONACK(1),OneNRegularRegister.PL)) // Estos paquetes tienen ids distintos a los q se enviaron.
        .deliver(Packet(p2,p0,ONACK(1),OneNRegularRegister.PL))
        .indications shouldBe Set(ONRRWriteReturn)
    }

    "E" in {
      ps(0).request(ONRRWrite(1))
        .deliver(Packet(p0,p0,ONACK(1),OneNRegularRegister.PL))
        .deliver(Packet(p1,p0,ONACK(1),OneNRegularRegister.PL))
        .request(ONRRRead)
        .deliver(Packet(p0,p0,ONVALUE(1,1,Some(1)),OneNRegularRegister.PL))
        .deliver(Packet(p1,p0,ONVALUE(1,2,Some(5)),OneNRegularRegister.PL))
        .indications shouldBe Set(ONRRReadReturn(5))
    }

    "F" in {
      val f = OneNRegularRegister.processLocal[Int](all.size,p0)
      val init = ONRRState.init[Int](p0,all,Int.MaxValue)
      val LocalStep(indications,events,requests,sends,ONRRState(state,_)) = f(PublicRequest(ONRRRead),init)
      indications should be(empty)
      events should be(empty)
      sends should be(empty)
      requests.toList should matchPattern { case LocalRequest(Left(BebBcast(Payload(_, ONREAD(1)), OneNRegularRegister.BEB))) :: Nil =>  }
      state shouldBe ONRRStateI(None, 0, 0, 0, 1, Map.empty)
    }
  }

  // TODO hay dos tipos de test para hacer. Los más simples serian pasar un request/indication/localrequest... y un estado y ver q nos devuelve
  // seria probar la funcion processLocal. Estos test no me garantizan q el algoritmo de shared memory este bien!
  // este es el segundo tipo de test que implica tener un cluster, no un unico proceso!
  // el unico peligro de estos dos tipos de test es q no tengo control sobre los mensajes q envio, es decir si los puedo generar libremente
  // por fuera de la logica q los genera, podria estar probando casos invalidos?!, tal vez el mensaje en si no es invalido pero es invalido en
  // el estado actual del proceso.

  // para los test del segundo tipo tendria q hacer alguna clase q me deje pasarle un schedule de actiones (aunque el problema es q no conozco los uuid, pero puedo especificar
  // el resto del mensaje (from,to,payload) y esperar q no haya duplicados (de ultima tirar una excepcion y q me obligue a escribir el test de otra manera).


}
