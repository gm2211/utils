package com.gm2211.collections

import com.gm2211.serialization.Yaml
import com.gm2211.template.HandleBarPopulator

import scala.io.Source

final class HandleBarPopulatorSpec extends BaseSpec {

  "The handlebar populator" should "correctly replace placeholders" in {
    val templateString = readResource("template-overrides.yml")
    val templateProperties: Map[String, Any] = Yaml.readFileAsMap(resource("template-overrides.yml"))

    val populatedTemplateString = HandleBarPopulator.populateTemplate(templateString, templateProperties)
    val populatedTemplate: Map[String, Any] = Yaml.readValueAsMap(populatedTemplateString)

    populatedTemplate should contain key "var/conf/install.yml"
    populatedTemplate("var/conf/install.yml").asInstanceOf[Map[String, Any]] should contain ("serverPort" -> 8080)

    val stream = Source.fromURL(resourceUrl("template-overrides.yml")).toStream
  }

}
