package jsoft.graphql.model

trait RendererFactory {
  def supply(): Renderer
}
