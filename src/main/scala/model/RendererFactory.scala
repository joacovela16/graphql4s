package model

trait RendererFactory {
  def supply(): Renderer
}
