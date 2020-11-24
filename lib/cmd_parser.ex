defmodule CmdParser do
  def parse(<<"login", _args :: binary>>), do: {:ok, :login, []}
  def parse(<<"show", _args :: binary>>), do: {:ok, :show, []}
  def parse(<<"chat", _args :: binary>>), do: {:ok, :chat, []}
  def parse(_), do: {:error, :bad_command}
end
