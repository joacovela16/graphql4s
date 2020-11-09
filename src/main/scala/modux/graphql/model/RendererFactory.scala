package modux.graphql.model

trait RendererFactory {
  def supply(): Renderer
}
