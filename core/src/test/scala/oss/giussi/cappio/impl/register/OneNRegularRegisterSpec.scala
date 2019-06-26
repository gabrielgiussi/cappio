package oss.giussi.cappio.impl.register

import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.impl.net.FairLossLink.FLLSend
import oss.giussi.cappio.impl.register.OneNRegularRegister.{ONREAD, ONRRRead}
import oss.giussi.cappio.{CappIOSpec, Packet, ProcessId}

class OneNRegularRegisterSpec extends CappIOSpec {


  val all = (0 to 2).map(ProcessId)
  val timeout = 100
  val N = all.size
  val ps = all.map(id => id.id -> OneNRegularRegister.init[Int](id, N, timeout, all.toSet)).toMap

  "A" should {
    "B" in {
      ps(0).request(ONRRRead).send.map { case FLLSend(Packet(_, ONREAD(1), ProcessId(0), to, OneNRegularRegister.BEB)) => to } shouldBe all.toSet
    }

    "C" in {
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
