defmodule ByteStreamTest do
  use ExUnit.Case
  alias Lobby.Utils.ByteStream
  alias Lobby.Utils.Queue

  describe "ByteStream" do
    test "push buffer" do
      stream = ByteStream.new()
      assert %ByteStream{buffers: %Queue{length: 0}, remaining: 0} = stream

      stream = ByteStream.push_buffer(stream, <<>>)
      assert %ByteStream{buffers: %Queue{length: 0}, remaining: 0} = stream

      stream = ByteStream.push_buffer(stream, <<1, 2, 3, 4, 5>>)
      assert %ByteStream{buffers: %Queue{length: 1}, remaining: 5} = stream

      stream = ByteStream.push_buffer(stream, <<1, 2, 3>>)
      assert %ByteStream{buffers: %Queue{length: 2}, remaining: 8} = stream
    end

    test "remaining" do
      stream = ByteStream.new()

      stream = ByteStream.push_buffer(stream, <<1, 2, 3>>)
      assert ByteStream.remaining(stream) == 3

      stream = ByteStream.push_buffer(stream, <<4, 5>>)
      assert ByteStream.remaining(stream) == 5
    end

    test "peek" do
      stream = ByteStream.new()
      stream = ByteStream.push_buffer(stream, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>)

      assert %ByteStream{
               buffers: %Queue{inner: {[<<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>], []}, length: 1},
               remaining: 10
             } = stream

      assert {<<1, 2, 3, 4>>, ^stream} = ByteStream.peek(stream, 4)

      stream = ByteStream.new()
      stream = ByteStream.push_buffer(stream, <<1, 2, 3, 4, 5>>)
      stream = ByteStream.push_buffer(stream, <<6, 7, 8>>)
      stream = ByteStream.push_buffer(stream, <<9, 10>>)

      assert %ByteStream{
               buffers: %Queue{inner: {[<<9, 10>>, <<6, 7, 8>>], [<<1, 2, 3, 4, 5>>]}, length: 3},
               remaining: 10
             } = stream

      assert {<<1, 2, 3, 4>>, ^stream} = ByteStream.peek(stream, 4)
      assert {<<>>, ^stream} = ByteStream.peek(stream, 0)
      assert {:insufficient, ^stream} = ByteStream.peek(stream, 111)
    end

    test "read" do
      stream = ByteStream.new()
      stream = ByteStream.push_buffer(stream, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>)

      assert %ByteStream{
               buffers: %Queue{inner: {[<<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>], []}, length: 1},
               remaining: 10
             } = stream

      assert {<<1, 2, 3, 4>>,
              %ByteStream{
                buffers: %Queue{inner: {[], [<<5, 6, 7, 8, 9, 10>>]}, length: 1},
                remaining: 6
              } = stream} = ByteStream.read(stream, 4)

      assert {<<5, 6>>,
              %ByteStream{
                buffers: %Queue{inner: {[], [<<7, 8, 9, 10>>]}, length: 1},
                remaining: 4
              } = stream} = ByteStream.read(stream, 2)

      assert {<<7, 8, 9, 10>>,
              %ByteStream{
                buffers: %Queue{inner: {[], []}, length: 0},
                remaining: 0
              } = stream} = ByteStream.read(stream, 4)

      assert {:insufficient,
              %ByteStream{
                buffers: %Queue{inner: {[], []}, length: 0},
                remaining: 0
              } = stream} = ByteStream.read(stream, 1)

      stream = ByteStream.new()
      stream = ByteStream.push_buffer(stream, <<1, 2, 3, 4, 5>>)
      stream = ByteStream.push_buffer(stream, <<6, 7, 8>>)
      stream = ByteStream.push_buffer(stream, <<9, 10>>)

      assert %ByteStream{
               buffers: %Queue{inner: {[<<9, 10>>, <<6, 7, 8>>], [<<1, 2, 3, 4, 5>>]}, length: 3},
               remaining: 10
             } = stream

      assert {<<1, 2, 3, 4>>,
              %ByteStream{
                buffers: %Queue{inner: {[<<9, 10>>], [<<5>>, <<6, 7, 8>>]}, length: 3},
                remaining: 6
              } = stream} = ByteStream.read(stream, 4)

      assert {<<5, 6>>,
              %ByteStream{
                buffers: %Queue{inner: {[<<9, 10>>], [<<7, 8>>]}, length: 2},
                remaining: 4
              } = stream} = ByteStream.read(stream, 2)

      assert {<<7, 8, 9>>,
              %ByteStream{
                buffers: %Queue{inner: {[], [<<10>>]}, length: 1},
                remaining: 1
              } = stream} = ByteStream.read(stream, 3)

      assert {<<10>>,
              %ByteStream{
                buffers: %Queue{inner: {[], []}, length: 0},
                remaining: 0
              } = stream} = ByteStream.read(stream, 1)

      assert {:insufficient,
              %ByteStream{
                buffers: %Queue{inner: {[], []}, length: 0},
                remaining: 0
              } = stream} = ByteStream.read(stream, 1)
    end
  end
end
