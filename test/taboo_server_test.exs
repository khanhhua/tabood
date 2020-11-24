defmodule TabooServerTest do
  use ExUnit.Case
  doctest TabooServer

  test "greets the world" do
    assert TabooServer.hello() == :world
  end
end
