package com.wix.sms.clickatell.testkit

import java.util.concurrent.atomic.AtomicReference

import akka.http.scaladsl.model._
import com.wix.e2e.http.RequestHandler
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory.aMockWebServerWith
import com.wix.sms.clickatell.model._
import com.wix.sms.clickatell.{ClickatellHelper, Credentials}
import com.wix.sms.model.Sender

class ClickatellDriver(port: Int) {
  private val delegatingHandler: RequestHandler = { case r: HttpRequest => handler.get().apply(r) }
  private val notFoundHandler: RequestHandler = { case _: HttpRequest => HttpResponse(status = StatusCodes.NotFound) }

  private val handler = new AtomicReference(notFoundHandler)

  private val probe = aMockWebServerWith(delegatingHandler).onPort(port).build
  private val requestParser = new MessageRequestParser
  private val responseParser = new MessageResponseParser

  def startProbe() {
    probe.start()
  }

  def stopProbe() {
    probe.stop()
  }

  def resetProbe() {
    handler.set(notFoundHandler)
  }

  def aMessageFor(credentials: Credentials, sender: Sender, destPhone: String, text: String): MessageCtx = {
    new MessageCtx(
      credentials = credentials,
      sender = sender,
      destPhone = destPhone,
      text = text)
  }

  private def prependHandler(handle: RequestHandler) =
    handler.set(handle orElse handler.get())

  class MessageCtx(credentials: Credentials, sender: Sender, destPhone: String, text: String) {
    private val expectedRequest = ClickatellHelper.createMessageRequest(
      sender = sender,
      destPhone = destPhone,
      text = text
    )

    def returns(msgId: String): Unit = {
      val response = new MessageResponse(
        data = Some(Data(
          message = Seq(Message(
            accepted = true,
            to = destPhone,
            apiMessageId = msgId
          ))
        ))
      )

      val responseJson = responseParser.stringify(response)
      respondWith(HttpResponse(
        status = StatusCodes.OK,
        entity = HttpEntity(ContentTypes.`application/json`, responseJson))
      )
    }

    def failsWith(code: String, description: String): Unit = {
      val response = new MessageResponse(
        error = Some(Error(
          code = code,
          description = description,
          documentation = s"http://www.clickatell.com/help/apidocs/error/$code.htm"
        ))
      )

      val responseJson = responseParser.stringify(response)
      respondWith(HttpResponse(
        status = StatusCodes.BadRequest,
        entity = HttpEntity(ContentTypes.`application/json`, responseJson))
      )
    }

    def isUnauthorized(): Unit = {
      respondWith(HttpResponse(
        status = StatusCodes.Unauthorized,
        entity = HttpEntity.Empty)
      )
    }

    private def respondWith(httpResponse: HttpResponse): Unit = {
      prependHandler({
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path("/message"),
        headers,
        entity,
        _) if isStubbedRequestEntity(entity) && isStubbedHeaders(headers) => httpResponse
      })
    }

    private def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      val requestJson = entity.extractAsString
      val request = requestParser.parse(requestJson)

      request == expectedRequest
    }

    private def isStubbedHeaders(headers: Seq[HttpHeader]): Boolean = {
      isAuthorized(headers) && isCorrectVersion(headers)
    }

    private def isAuthorized(headers: Seq[HttpHeader]): Boolean = {
      val expectedAuthorizationValue = s"Bearer ${credentials.accessToken}"

      headers.exists { header =>
        header.name.equalsIgnoreCase("Authorization") && header.value == expectedAuthorizationValue
      }
    }

    private def isCorrectVersion(headers: Seq[HttpHeader]): Boolean = {
      headers.exists { header =>
        header.name.equalsIgnoreCase(Headers.version) && header.value == Versions.`1`
      }
    }
  }
}
