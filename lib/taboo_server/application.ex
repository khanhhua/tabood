defmodule TabooServer.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Starts a worker by calling: TabooServer.Worker.start_link(arg)
      {TabooServer, [{127,0,0,1}, 1818]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: TabooServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
