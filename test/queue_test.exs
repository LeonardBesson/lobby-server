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
      queue = Queue.push_back(queue, 1)
      assert %Queue{inner: {[1], []}, length: 1} = queue
      queue = Queue.push_back(queue, 2)
      assert %Queue{inner: {[2], [1]}, length: 2} = queue
    end

    test "push front" do
      queue = Queue.new()
      assert %Queue{inner: {[], []}, length: 0} = queue
      queue = Queue.push_front(queue, 1)
      assert %Queue{inner: {[], [1]}, length: 1} = queue
      queue = Queue.push_front(queue, 2)
      assert %Queue{inner: {[1], [2]}, length: 2} = queue
    end

    test "pop front" do
      queue = Queue.new()
      assert %Queue{inner: {[], []}, length: 0} = queue
      queue = Queue.push_back(queue, 1)
      assert %Queue{inner: {[1], []}, length: 1} = queue
      queue = Queue.push_back(queue, 2)
      assert %Queue{inner: {[2], [1]}, length: 2} = queue

      assert {1, %Queue{inner: {[], [2]}, length: 1} = queue} = Queue.pop_front(queue)
      assert {2, %Queue{inner: {[], []}, length: 0} = queue} = Queue.pop_front(queue)
      assert {nil, %Queue{inner: {[], []}, length: 0} = queue} = Queue.pop_front(queue)

      queue = Queue.push_back(queue, 3)
      assert %Queue{inner: {[3], []}, length: 1} = queue
      assert {3, %Queue{inner: {[], []}, length: 0} = queue} = Queue.pop_front(queue)
    end

    test "pop back" do
      queue = Queue.new()
      assert %Queue{inner: {[], []}, length: 0} = queue
      queue = Queue.push_front(queue, 1)
      assert %Queue{inner: {[], [1]}, length: 1} = queue
      queue = Queue.push_front(queue, 2)
      assert %Queue{inner: {[1], [2]}, length: 2} = queue

      assert {1, %Queue{inner: {[2], []}, length: 1} = queue} = Queue.pop_back(queue)
      assert {2, %Queue{inner: {[], []}, length: 0} = queue} = Queue.pop_back(queue)
      assert {nil, %Queue{inner: {[], []}, length: 0} = queue} = Queue.pop_back(queue)

      queue = Queue.push_front(queue, 3)
      assert %Queue{inner: {[], [3]}, length: 1} = queue
      assert {3, %Queue{inner: {[], []}, length: 0} = queue} = Queue.pop_back(queue)
    end

    test "length" do
      queue = Queue.new()
      assert %Queue{inner: {[], []}, length: 0} = queue
      queue = Queue.push_back(queue, 1)
      assert %Queue{inner: {[1], []}, length: 1} = queue
      queue = Queue.push_back(queue, 2)
      assert %Queue{inner: {[2], [1]}, length: 2} = queue

      assert Queue.length(queue) == 2

      {_, queue} = Queue.pop_front(queue)
      assert Queue.length(queue) == 1
      {_, queue} = Queue.pop_front(queue)
      assert Queue.length(queue) == 0
    end
  end
end
