defmodule TabooGame do
  @behaviour :gen_statem

  require Record
  Record.defrecord :game_data, [player_ids: []]

  def start_link do
    :gen_statem.start_link(__MODULE__, [], [])
  end

  def start do
    :gen_statem.start(__MODULE__, [], [])
  end

  def init([]) do
    {:ok, :awaiting_players,
      game_data(player_ids: [])
    }
  end

  @spec terminate(any, any, any) :: :ok
  def terminate(reason, _state, _data) do
    IO.puts("Game terminated")
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

  def awaiting_players(_event_type, _event, _state) do
    :keep_state_and_data
  end
end
