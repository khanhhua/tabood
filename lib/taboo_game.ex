defmodule TabooGame do
  use GenStateMachine, callback_mode: :state_functions

  require Record
  require CmdParser, as: Parser
  Record.defrecord :client_data, [sockets: [], word: nil, active_player: nil]

  @spec start_link :: :ignore | {:error, any} | {:ok, pid}
  def start_link() do
    name = {:via, Registry, {Registry.GameRegistry, "game1"}}
    GenStateMachine.start_link(__MODULE__, [], name: name)
  end

  @spec from_registry(any) :: :error | {:ok, pid}
  def from_registry(name) do
    case Registry.lookup(Registry.GameRegistry, name) do
      [{pid, _}] -> {:ok, pid}
      _ -> :error
    end
  end

  def init([]) do
    Process.flag(:trap_exit, true)
    {:ok, :awaiting_players, client_data()}
  end

  def join(pid, client_socket), do: GenStateMachine.call(pid, {:join, client_socket})

  # STATE CALLBACK FUNCTIONS

  def awaiting_players(:info, {:tcp, _socket, cmdline}, state) do
    IO.inspect cmdline, label: "CMD"
    case Parser.parse(cmdline) do
      {:cmd, fun, params} when fun == :chat ->
        case apply(__MODULE__, fun, [state] ++ params) do
          {:ok, state_name, next_state} ->
            {:next_state, state_name, next_state}
          _ -> :keep_state_and_data
        end
      _ -> :keep_state_and_data
    end
  end
  def awaiting_players(:info, {:tcp_closed, socket}, state) do
    sockets = client_data(state, :sockets)
        |> Enum.filter(&(&1 != socket))
    {:next_state, :awaiting_players, client_data(state, sockets: sockets)}
  end
  def awaiting_players({:call, from_ref}, {:join, client_socket}, state) do
    next_state = accept_client_socket(state, client_socket)
    case length(client_data(next_state, :sockets)) > 1 do
      true ->
        next_state = next_state
            |> allocate_word
            |> set_active_player
        {:next_state, :in_game, next_state, [{:reply, from_ref, :ok}]}
      false -> {:next_state, :awaiting_players, next_state, [{:reply, from_ref, :ok}]}
    end
  end

  @spec in_game(any, any, any) :: :keep_state_and_data | {:next_state, any, any}
  def in_game({:call, from_ref}, {:join, client_socket}, state) do
    next_state = accept_client_socket(state, client_socket)
    {:next_state, :in_game, next_state, [{:reply, from_ref, :ok}]}
  end

  def in_game(:info, {:tcp, socket, cmdline}, state) do
    case Parser.parse(cmdline) do
      {:cmd, :text, text} ->
        cond do
          socket == client_data(state, :active_player) ->
            # Make sure "text" is not the word itself
            broadcast_text(state, socket, text)
            :keep_state_and_data
          socket != client_data(state, :active_player) ->
            # Mask "text" as **** if text is the word itself
            broadcast_text(state, socket, text)
            cond do
              text == client_data(state, :word) ->
                next_state = state
                    |> allocate_word
                    |> set_active_player
                {:next_state, :in_game, next_state}
              true -> :keep_state_and_data
            end
        end
      {:cmd, fun, params} when fun == :chat ->
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
  def accept_client_socket(state, client_socket) do
    :inet.setopts(client_socket, [:binary, {:active, true}, {:packet, :raw}])
    sockets = client_data(state, :sockets) ++ [client_socket]
    IO.inspect sockets, label: "sockets"
    client_data(state, sockets: sockets)
  end
  def chat(state, text) do
    IO.write "Chat: #{text}\n"
    {:ok, :in_game, state}
  end

  def allocate_word(state) do
    client_data(state, word: "OS")
  end

  def set_active_player(state) do
    sockets = client_data(state, :sockets)
    case client_data(state, :active_player) do
      nil ->
        [active_player|_] = sockets
        client_data(state, active_player: active_player)
      active_player ->
        index = Enum.find_index(sockets, &(&1 == active_player))
        active_player = Enum.at(sockets, rem(index + 1, length(sockets)))
        client_data(state, active_player: active_player)
    end
  end

  @spec broadcast_text(tuple, any, any) :: :ok
  def broadcast_text(state, from, text) do
    client_data(state, :sockets)
        |> Enum.filter(&(&1 != from))
        |> Enum.each(&(:gen_tcp.send(&1, "#{text}\n")))
  end
end
