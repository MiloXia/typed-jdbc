package pool

import java.io.File

import com.typesafe.config.{Config, ConfigFactory => TypeSafeConfigFactory}

/**
  * Created by Admin on 2017/4/21.
  */
object ConfigFactory {

  private val defaultConfigName = "default.conf"
  private val localConfigName = "local.conf"
  private val applicationConfigName = "application.conf"

  def load: Config = {
    def withApplicationConfig(config: Config) = config.withFallback(TypeSafeConfigFactory.parseResourcesAnySyntax(applicationConfigName))
      .withFallback(TypeSafeConfigFactory.load())
    sys.props.get("config.file") match {
      case Some(configFileName) =>
        val f = new File(configFileName)
        if(!f.canRead) {
          val error = s"file ${f.getAbsolutePath} cannot be read!"
          throw new Exception(error)
        }
        println("DEUBG - load config file: " + f.getAbsolutePath)
        withApplicationConfig(TypeSafeConfigFactory.parseFileAnySyntax(f)).resolve()
      case _ =>
        val localConfig = TypeSafeConfigFactory.parseResourcesAnySyntax(localConfigName)
        if(!localConfig.root.entrySet.isEmpty)
          println("DEUBG - load local config resource: " + localConfigName)
        val defaultConfig = TypeSafeConfigFactory.parseResourcesAnySyntax(defaultConfigName)
        println("DEUBG - load default config resource: " + defaultConfigName)
        withApplicationConfig(localConfig.withFallback(defaultConfig)).resolve()
    }
  }

  lazy val config = load
}
