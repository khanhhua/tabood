defmodule TabooClient do
  @behaviour :gen_statem

  require Record
  require CmdParser, as: Parser
  Record.defrecord :client_data, [socket: nil, username: nil, game_pid: nil]

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(client_socket) do
    :gen_statem.start_link(__MODULE__, client_socket, [])
  end

  @spec init(port) :: {:ok, term(), {:client_data, port, nil, nil}}
  def init(client_socket) do
    Process.flag(:trap_exit, true)
    :inet.setopts(client_socket, [:binary, {:active, true}, {:packet, :raw}])
    {:ok, :connected, client_data(socket: client_socket)}
  end

  @spec terminate(any, any, any) :: :ok
  def terminate(reason, _state, _data) do
    IO.puts("Client terminated")
    IO.inspect reason, label: "Reason"
    :ok
  end

  def code_change(_vsn, state, data, _extra) do
    {:ok, state, data}
  end

  @spec callback_mode :: :handle_event_function | :state_functions | [:handle_event_function | :state_enter | :state_functions]
  def callback_mode do
    # Very important
    :state_functions
  end

  def connected(:info, {:tcp, socket, cmdline}, state) do
    case Parser.parse(cmdline) do
      {:ok, fun, params} when fun == :login ->
        case apply(__MODULE__, fun, [state] ++ params) do
          {:ok, state_name, next_state} ->
            {:next_state, state_name, next_state}
          _ -> :keep_state_and_data
        end
      _ -> :keep_state_and_data
    end
  end
  def connected(_event_type, _event, _state) do
    :keep_state_and_data
  end

  def in_lobby({:call, from_ref}, {:join, game_pid}, state) do
    case client_data(state, :game_pid) == nil do
      true -> {:next_state, :in_game, client_data(state, game_pid: game_pid)}
      false -> :keep_state_and_data
    end
  end
  def in_lobby(_event_type, _event, _state) do
    :keep_state_and_data
  end

  def in_game(:info, {:tcp, socket, cmdline}, state) do
    case Parser.parse(cmdline) do
      {:ok, fun, params} when fun == :chat ->
        case apply(__MODULE__, fun, [state] ++ params) do
          {:ok, state_name, next_state} ->
            {:next_state, state_name, next_state}
          _ -> :keep_state_and_data
        end
      _ -> :keep_state_and_data
    end
  end
  def in_game(_event_type, _event, _state) do
    :keep_state_and_data
  end

  # Private functions, all takes (state, arg1, ...) and return next state
  def login(state, username) do
    # Sequences of actions upon login
    # 1. Check for username
    # 2. If allowed, look for a room for user
    # 3. If room not found, create a new room - let the room itself invite guests
    {:ok, :in_lobby, client_data(state, username: username)}
  end

  def chat(state, text) do
    {:ok, :in_game, state}
  end
end
