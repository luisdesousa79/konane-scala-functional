
// Application é a classe base de uma aplicação JavaFX
import javafx.application.Application

// Scene é o conteúdo da janela
import javafx.scene.Scene

// container ou layout
import javafx.scene.layout.BorderPane

// Stage é a janela principal
import javafx.stage.Stage

// Classe GUI
// Responsável por:
//
//iniciar JavaFX;
//criar a janela;
//montar os componentes visuais.
class GUI extends Application {

  // método que JavaFC chama automaticamente quando a aplicação começa
  override def start(stage: Stage): Unit = {
    stage.setTitle("Konane")
    stage.show()
  }
}
