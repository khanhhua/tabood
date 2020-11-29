defmodule CmdParser do
  def parse(<<"/login ", args :: binary>>), do: {:cmd, :login, String.split(args)}
  def parse(<<"/show ", args :: binary>>), do: {:cmd, :show, String.split(args)}
  def parse(<<"/chat ", args :: binary>>), do: {:cmd, :chat, String.split(args)}
  def parse(<<"/", _args :: binary>>), do: {:error, :bad_command}
  def parse(text), do: {:cmd, :text, String.trim(text)}
end
