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

import java.nio.file.Paths

import better.files._
import nl.knaw.dans.easy.validatebag.rules.bagit.bagIsValid
import nl.knaw.dans.easy.validatebag.validation.RuleViolationDetailsException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatest.{ BeforeAndAfterEach, Inside }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex
import scala.util.{ Failure, Success }

trait TestSupportFixture extends AnyFlatSpec with Matchers with Inside with BeforeAndAfterEach with DebugEnhancedLogging {
  lazy val testDir: File = File(s"target/test/${ getClass.getSimpleName }")
  val ddmSchemaUrl = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
  val filesSchemaUrl = "https://easy.dans.knaw.nl/schemas/bag/metadata/files/2018/04/files.xsd"
  val metadataSchemaUrl = "https://easy.dans.knaw.nl/schemas/bag/metadata/agreements/2018/12/agreements.xsd"

  protected val bagsDir: File = Paths.get("src/test/resources/bags")

  implicit val isReadable: File => Boolean = _.isReadable

  private def shouldBeValidAccordingToBagIt(inputBag: String): Unit = {
    bagIsValid(new TargetBag(bagsDir / inputBag, 0)) shouldBe a[Success[_]] // Profile version does not matter here
  }

  protected def testRuleViolationRegex(rule: Rule, inputBag: String, includedInErrorMsg: Regex, profileVersion: ProfileVersion = 0, doubleCheckBagItValidity: Boolean = true): Unit = {
    val result = rule(new TargetBag(bagsDir / inputBag, profileVersion))
    if (doubleCheckBagItValidity) shouldBeValidAccordingToBagIt(inputBag)
    result shouldBe a[Failure[_]]
    inside(result) {
      case Failure(e: RuleViolationDetailsException) =>
        e.getMessage should include regex includedInErrorMsg
    }
  }

  protected def testRuleViolation(rule: Rule, inputBag: String, includedInErrorMsg: String, profileVersion: ProfileVersion = 0, doubleCheckBagItValidity: Boolean = true): Unit = {
    val result = rule(new TargetBag(bagsDir / inputBag, profileVersion))
    if (doubleCheckBagItValidity) shouldBeValidAccordingToBagIt(inputBag)
    result shouldBe a[Failure[_]]
    inside(result) {
      case Failure(e: RuleViolationDetailsException) =>
        e.getMessage should include(includedInErrorMsg)
      case Failure(e) => fail(s"Not the expected type of exception: $e")
    }
  }

  protected def testRuleSuccess(rule: Rule, inputBag: String, profileVersion: ProfileVersion = 0, doubleCheckBagItValidity: Boolean = true): Unit = {
    if (doubleCheckBagItValidity) shouldBeValidAccordingToBagIt(inputBag)
    rule(new TargetBag(bagsDir / inputBag, profileVersion)) shouldBe a[Success[_]]
  }
}
