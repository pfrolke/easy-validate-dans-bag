/**
 * Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.validatebag

import java.net.{ HttpURLConnection, URL }

import org.scalatest.matchers.should.Matchers

import scala.util.Try

trait CanConnectFixture {
  this: Matchers =>

  def assumeCanConnect(urls: String*): Unit = {
    assume(Try {
      urls.map(url => {
        new URL(url).openConnection match {
          case connection: HttpURLConnection =>
            connection.setConnectTimeout(5000)
            connection.setReadTimeout(5000)
            connection.connect()
            connection.disconnect()
            true
          case connection => throw new IllegalArgumentException(s"unknown connection type: $connection")
        }
      })
    }.isSuccess)
  }
}