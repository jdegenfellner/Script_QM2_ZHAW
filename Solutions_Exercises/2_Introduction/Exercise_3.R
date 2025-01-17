library(DiagrammeR)

grViz("
digraph model {
  graph [layout = dot, rankdir = TB]

  # Nodes for variables
  node [shape = ellipse, style = filled, fillcolor = lightblue]
  alpha [label = 'α ~ Normal(171.1, 20)']
  beta [label = 'β ~ Normal(0, 10)']
  sigma [label = 'σ ~ Uniform(0, 50)']
  mu [label = 'μ_i = α + β(x_i - x̄)']
  h_i [label = 'h[i] ~ Normal(μ_i, σ)']

  # Connections
  alpha -> mu
  beta -> mu
  mu -> h_i
  sigma -> h_i
}
")
