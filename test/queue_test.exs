defmodule QueueTest do
  use ExUnit.Case
  alias Lobby.Utils.Queue

  describe "Queue" do
    test "new" do
      assert %Queue{inner: {[], []}, length: 0} = Queue.new()
    end

    test "push back" do
      queue = Queue.new()
      assert %Queue{inner: {[], []}, length: 0} = queue
      queue = queue |> Queue.push_back(1)
      assert %Queue{inner: {[1], []}, length: 1} = queue
      queue = queue |> Queue.push_back(2)
      assert %Queue{inner: {[2], [1]}, length: 2} = queue
    end

    test "pop front" do
      queue = Queue.new()
      assert %Queue{inner: {[], []}, length: 0} = queue
      queue = queue |> Queue.push_back(1)
      assert %Queue{inner: {[1], []}, length: 1} = queue
      queue = queue |> Queue.push_back(2)
      assert %Queue{inner: {[2], [1]}, length: 2} = queue

      assert {1, %Queue{inner: {[], [2]}, length: 1} = queue} = queue |> Queue.pop_front()
      assert {2, %Queue{inner: {[], []}, length: 0} = queue} = queue |> Queue.pop_front()
      assert {nil, %Queue{inner: {[], []}, length: 0} = queue} = queue |> Queue.pop_front()

      queue = queue |> Queue.push_back(3)
      assert %Queue{inner: {[3], []}, length: 1} = queue
      assert {3, %Queue{inner: {[], []}, length: 0} = queue} = queue |> Queue.pop_front()
    end

    test "length" do
      queue = Queue.new()
      assert %Queue{inner: {[], []}, length: 0} = queue
      queue = queue |> Queue.push_back(1)
      assert %Queue{inner: {[1], []}, length: 1} = queue
      queue = queue |> Queue.push_back(2)
      assert %Queue{inner: {[2], [1]}, length: 2} = queue

      assert queue |> Queue.length() == 2

      {_, queue} = queue |> Queue.pop_front()
      assert queue |> Queue.length() == 1
      {_, queue} = queue |> Queue.pop_front()
      assert queue |> Queue.length() == 0
    end
  end
end
