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
package nl.knaw.dans.easy.validatebag.rules

import java.nio.file.Files

import nl.knaw.dans.easy.validatebag.BagDir

import scala.util.Try
import nl.knaw.dans.easy.validatebag.lib.ValidatorAPI._
import nl.knaw.dans.easy.validatebag.lib.InfoPackageType._
import nl.knaw.dans.easy.validatebag.lib.Rules

// this is a file where you put/register the rules
// note that it implements the Rules interface/trait
// you can have multiple of these files and merge them at a later point in time (see also Rules2 and RuleRunner)
object Rules1 extends Rules {

  val bagMustBeValid = (b: BagDir) => Try {
    // TODO: check that the bag is VALID according to BagIt.
  }
  val bagMustBeVirtuallyValid = (b: BagDir) => Try {
    // TODO: same als bagMustBeValid, but when NON-VALID warn that "virtually-only-valid" bags cannot not be recognized by the service yet.
  }
  val bagMustContainBagInfoTxt = (b: BagDir) => Try {}

  def bagInfoTxtMustContainBagItProfileVersion(version: String)(b: BagDir) = Try {}

  val bagMustContainMetadataDir = (b: BagDir) => Try {
    if (Files.isDirectory(b.resolve("metadata"))) ()
    else fail("Mandatory directory 'metadata' not found in bag.")
  }

  override val rules = Map(
    // TODO: Add the rules to the respective rule bases
    0 -> Seq(
      numberedRule("1.1.1", bagMustBeValid, SIP),
      numberedRule("1.1.2", bagMustBeVirtuallyValid, AIP),
      numberedRule("1.2.1", bagMustContainBagInfoTxt),
      numberedRule("1.2.2", bagInfoTxtMustContainBagItProfileVersion("0.0.0")),
    ),
    1 -> Seq(
      numberedRule("2.1.1", bagMustBeValid, SIP),
    ),
  )
}
