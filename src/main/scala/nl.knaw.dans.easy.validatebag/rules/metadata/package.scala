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

import java.io.ByteArrayInputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import nl.knaw.dans.easy.validatebag
import nl.knaw.dans.easy.validatebag.{ TargetBag, XmlValidator }
import nl.knaw.dans.easy.validatebag.validation._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Success, Try }
import scala.xml._

package object metadata extends DebugEnhancedLogging {
  val namespaceSchemaInstance = new URI("http://www.w3.org/2001/XMLSchema-instance")
  val namespaceDcterms = new URI("http://purl.org/dc/terms/")

  def xmlFileMustConformToSchema(xmlFile: Path, validator: XmlValidator)(t: TargetBag): Try[Unit] = {
    trace(xmlFile)
    require(!xmlFile.isAbsolute, "Path to xmlFile must be relative.")
    (t.bagDir / xmlFile.toString).inputStream.map(validator.validate).map {
      _.recoverWith { case t: Throwable => Try(fail(t.getMessage)) }
    }
  }.get()

  def xmlFileMayConformToSchemaIfDefaultNamespace(validator: XmlValidator)(t: TargetBag): Try[Unit] = {
    trace(())
    t.tryFilesXml.map {
      xml =>
        if (xml.namespace == validatebag.filesXmlNamespace) {
          logger.debug("Validating files.xml against XML Schema")
          resource.managed(new ByteArrayInputStream(xml.toString.getBytes(StandardCharsets.UTF_8))).acquireAndGet { is =>
            validator.validate(is).recoverWith {
              case t: Throwable => Try(fail(t.getMessage))
            }
          }
        }
        else logger.info(s"files.xml does not declare namespace ${ validatebag.filesXmlNamespace }, NOT validating with XML Schema")
    }
  }

  def ddmMayContainDctermsLicenseFromList(ddmPath: Path, allowedLicenses: Seq[URI])(t: TargetBag): Try[Unit] = Try {
    trace(())
    val metadata = t.tryDdm.get \ "dcmiMetadata" // TODO: inside Try
    val licenses = (metadata \ "license").toList
    debug(s"Found licences: ${ licenses.mkString(", ") }")
    lazy val rightsHolders = (metadata \ "rightsHolder").toList

    licenses match {
      case license :: Nil if hasXsiType(namespaceDcterms, "URI")(license) =>
        val licenseUri = new URI(license.text)
        if (allowedLicenses.contains(licenseUri)) {
          if (rightsHolders.isEmpty) fail("Valid license found, but no rightsHolder specified")
          else Success(())
        }
        else fail(s"Found unknown or unsupported license: $licenseUri")
      case Nil | _ :: Nil =>
        debug("No licences with xsi:type=\"dcterms:URI\"")
        Success(())
      case _ => fail(s"Found ${ licenses.size } dcterms:license elements. Only one license is allowed.")
    }
  }

  private def hasXsiType(attrNamespace: URI, attrValue: String)(e: Node): Boolean = {
    e.attribute(namespaceSchemaInstance.toString, "type")
      .exists {
        case Seq(n) =>
          n.text.split(":") match {
            case Array(pref, label) => e.getNamespace(pref) == attrNamespace.toString && label == attrValue
            case _ => false
          }
        case _ => false
      }
  }

  def ddmDaisMustBeValid(t: TargetBag): Try[Unit] = {
    for {
      ddm <- t.tryDdm
      _ <- daisAreValid(ddm)
    } yield ()
  }

  private def daisAreValid(ddm: Elem): Try[Unit] = Try {
    val dais = (ddm \\ "DAI").filter(_.namespace == validatebag.dcxDaiNamespace)
    logger.debug(s"DAIs to check: ${ dais.mkString(", ") }")
    val invalidDais = dais.map(_.text).filterNot(s => digest(s.slice(0, s.length - 1), 9) == s.last)
    if (invalidDais.nonEmpty) fail(s"Invalid DAIs: ${ invalidDais.mkString(", ") }")
  }

  // Calculated the check digit of a DAI. Implementation copied from easy-ddm.
  private def digest(message: String, modeMax: Int): Char = {
    val reverse = message.reverse
    var f = 2
    var w = 0
    var mod = 0
    mod = 0
    while ( { mod < reverse.length }) {
      val cx = reverse.charAt(mod)
      val x = cx - 48
      w += f * x
      f += 1
      if (f > modeMax) f = 2

      { mod += 1; mod }
    }
    mod = w % 11
    if (mod == 0) '0'
    else {
      val c = 11 - mod
      if (c == 10) 'X'
      else (c + 48).toChar
    }
  }

  def ddmGmlPolygonPosListMustMeetExtraConstraints(t: TargetBag): Try[Unit] = {
    trace(())
    for {
      ddm <- t.tryDdm
      posLists <- getPolygonPosLists(ddm)
      _ <- posLists.map(validatePosList).collectResults.recoverWith {
        case ce: CompositeException => Try(fail(ce.getMessage))
      }
    } yield ()
  }

  private def getPolygonPosLists(parent: Elem): Try[Seq[Node]] = Try {
    trace(())
    val polygons = getPolygons(parent)
    polygons.flatMap(_ \\ "posList")
  }

  private def getPolygons(parent: Elem) = (parent \\ "Polygon").filter(_.namespace == validatebag.gmlNamespace)

  private def validatePosList(node: Node): Try[Unit] = Try {
    trace(node)
    def offendingPosListMsg(values: Seq[String]) = s"(Offending posList starts with: ${ values.take(10).mkString(", ") }...)"

    val values = node.text.split("""\s+""").toList
    val numberOfValues = values.size
    if (numberOfValues % 2 != 0) fail(s"Found posList with odd number of values: $numberOfValues. ${ offendingPosListMsg(values) }")
    if (numberOfValues < 8) fail(s"Found posList with too few values (less than 4 pairs). ${ offendingPosListMsg(values) }")
    if (values.take(2) != values.takeRight(2)) fail(s"Found posList with unequal first and last pairs. ${ offendingPosListMsg(values) }")
  }

  def filesXmlHasDocumentElementFiles(t: TargetBag) = {
    trace(())
    t.tryFilesXml.map(xml => if (xml.label != "files") fail("files.xml: document element must be 'files'"))
  }

  def polygonsInSameMultiSurfaceMustHaveSameSrsName(t: TargetBag) = Try {
    trace(())
    for {
      ddm <- t.tryDdm
      multiSurfaces <- getMultiSurfaces(ddm)
      _ = multiSurfaces.map(validateMultiSurface)
    } yield ()
  }

  private def getMultiSurfaces(ddm: Elem) = Try {
    (ddm \\ "MultiSurface").filter(_.namespace == validatebag.gmlNamespace).asInstanceOf[Seq[Elem]]
  }

  private def validateMultiSurface(ms: Elem) = {
    val polygons = getPolygons(ms)
    if (polygons.map(_.attribute("srsName").map(_.text)).distinct.size == 1) Success(())
    else Try(fail("Found MultiSurface element containing polygons with different srsNames"))
  }

  def filesXmlHasOnlyFiles(t: TargetBag): Try[Unit] = Try {
    trace(())
    val files = t.tryFilesXml.get // TODO: inside Try
    val nonFiles = (files \ "*").filterNot(_.label == "file")
    if (nonFiles.nonEmpty) fail(s"files.xml: children of document element must only be 'file'. Found non-file elements: ${ nonFiles.mkString(", ") }")
  }

  def filesXmlFileElementsAllHaveFilepathAttribute(t: TargetBag): Try[Unit] = Try {
    trace(())
    val xml = t.tryFilesXml.get // TODO: inside Try
    val files = xml \ "file"
    if (files.map(_ \@ "filepath").size != files.size) fail("Not al 'file' elements have a 'filepath' attribute")
  }

  def filesXmlAllFilesDescribedOnce(t: TargetBag): Try[Unit] = Try {
    trace(())
    val xml = t.tryFilesXml.get // TODO: inside Try
    val files = xml \ "file"
    val pathsInFilesXmlList = files.map(_ \@ "filepath")
    val duplicatePathsInFilesXml = pathsInFilesXmlList.groupBy(identity).collect { case (k, vs) if vs.size > 1 => k }
    val noDuplicatesFound = duplicatePathsInFilesXml.isEmpty
    val pathsInFileXml = pathsInFilesXmlList.toSet
    val filesInBagPayload = (t.bagDir / "data").walk().filter(_.isRegularFile).toSet
    val payloadPaths = filesInBagPayload.map(t.bagDir.path relativize _).map(_.toString)
    val fileSetsEqual = pathsInFileXml == payloadPaths

    if (noDuplicatesFound && fileSetsEqual) ()
    else {
      val msg =
        (if (noDuplicatesFound) ""
         else s" - Duplicate filepaths found: $duplicatePathsInFilesXml\n") +
          (if (fileSetsEqual) ""
           else s" - Filepaths in files.xml not equal to files found in data folder. Difference: " +
             s"(only in bag: ${ payloadPaths.diff(pathsInFileXml) }, only in files.xml: " +
             s"${ pathsInFileXml.diff(payloadPaths) }\n")
      fail(s"files.xml: errors in filepath-attributes:\n$msg")
    }
  }

  def filesXmlAllFilesHaveFormat(t: TargetBag): Try[Unit] = Try {
    trace(())
    val xml = t.tryFilesXml.get // TODO: inside Try
    val files = xml \ "file"
    val allFilesHaveFormat = files.forall(_.child.exists(n =>
      xml.getNamespace(n.prefix) == "http://purl.org/dc/terms/" && n.label == "format"))
    if (!allFilesHaveFormat) fail("files.xml: not all <file> elements contain a <dcterms:format>")
  }

  def filesXmlFilesHaveOnlyDcTerms(t: TargetBag): Try[Unit] = Try {
    trace(())
    val xml = t.tryFilesXml.get // TODO: inside Try
    if (xml.namespace == validatebag.filesXmlNamespace) () // Already checked by XML Schema
    else {
      val files = xml \ "file"
      val hasOnlyDcTermsInFileElements = (xml \ "file" \ "_").forall {
        case n: Elem => xml.getNamespace(n.prefix) == "http://purl.org/dc/terms/" || xml.getNamespace(n.prefix) == ""
        case _ => true // Don't check non-element nodes
      }
      if (!hasOnlyDcTermsInFileElements) fail("files.xml: non-dcterms elements found in some file elements")
    }
  }
}
