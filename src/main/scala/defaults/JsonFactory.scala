package defaults


import model.{Renderer, RendererFactory}

case object JsonFactory extends RendererFactory {
  override def supply(): Renderer = JsonRenderer()
}
