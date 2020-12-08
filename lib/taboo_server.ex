defmodule TabooServer do
  @moduledoc """
  Documentation for TabooServer.
  """

  use GenServer
  require Record

  Record.defrecord :server_state, [socket: nil]

  @max_clients_per_game 4

  def start_link([ip, port]) do
    GenServer.start_link(__MODULE__, [ip, port], [])
  end

  def init([ip, port]) do
    opts = [
      :binary,
      {:packet, 0},
      {:reuseaddr, true},
      {:keepalive, true},
      {:backlog, 30},
      {:active, false},
      {:ip, ip}
    ]
    create_game()

    case :gen_tcp.listen(port, opts) do
      {:ok, listen_socket} ->
        :gen_server.cast(self(), :accept)
        {:ok, server_state(socket: listen_socket)}
      {:error, reason} -> {:stop, reason}
    end
  end

  def handle_cast(:accept, state) do
    IO.puts("Accepting...")
    IO.inspect state, label: "state"
    {:ok, client_socket} = :gen_tcp.accept(server_state(state, :socket))
    IO.inspect(client_socket, label: "Incoming connection")

    {:ok, pid} = TabooGame.from_registry(:game1)
    IO.inspect(pid, label: "45 Game PID")
    case :gen_tcp.controlling_process(client_socket, pid) do
      :ok ->
        GenServer.cast(self(), :accept)
        TabooGame.join(pid, client_socket)
        if TabooGame.count_clients(pid) >= @max_clients_per_game do
          create_game()
        end
      {:error, reason} ->
        IO.puts("Could not transfer control to client handler #{reason}")
    end
    {:noreply, state}
  end

  def create_game do
    IO.puts "Creating a new game"
    with {:ok, pid} <- TabooGame.from_registry(:game1) do
      TabooGame.unregister(pid)
    end

    {:ok, game_pid} = TabooGame.start_link()
    IO.inspect game_pid, label: "Game PID"
  end
end
