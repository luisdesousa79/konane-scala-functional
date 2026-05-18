import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class StartGame extends Application {
  override def start(primaryStage: Stage): Unit = { //Override da funcao start da aplicacao
    primaryStage.setTitle("Menu") // Colocamos o titulo My hello world
    val fxmlLoader = new FXMLLoader(getClass.getResource("MainMenu.fxml")) //obter as definições do KonaneGame.fxml
    val mainViewRoot: Parent = fxmlLoader.load() //fazemos load do KonaneGame.fxml
    val scene = new Scene(mainViewRoot) //colocamos a janela na "scene"
    primaryStage.setScene(scene) //colocamos a cena na primrary stage
    primaryStage.show() //fazemos o show que basicamente vai demonstrar a imagem
  }
}

object FxApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[StartGame], args: _*) //Start Point da Aplicação
  }
}
