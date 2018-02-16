defmodule SpinlockTest do
  use ExUnit.Case
  doctest Spinlock

  test "step (with width 3) should insert 1 after the 0 and set the position and last_value" do
    start = %{buffer: [0], position: 0, last_value: 0} 
    assert %{buffer: [0,1], position: 1, last_value: 1} == Spinlock.step(start, 3)
  end
  
  test "2 steps (width 3) should return 0,2,1" do
    assert %{buffer: [0,2,1], position: 1, last_value: 2} == Spinlock.step(Spinlock.step(Spinlock.init, 3), 3)
  end

  test "9 steps should return correct result for sample" do
    assert %{buffer: [0,9,5,7,2,4,3,8,6,1], position: 1, last_value: 9} == Spinlock.step_n(Spinlock.init, 3, 9)
  end

  test "2017 steps should return correct result for sample" do
    got = Spinlock.step_n(Spinlock.init, 3, 2017)
    #index = Enum.find(got, fn(x) -> x == 1512)
    # 1512  1134  151 (2017) 638  1513  851
    lst = Enum.drop_while(got.buffer, fn(x) -> x != 1512 end) |> Enum.take(7)
    assert [1512,1134,151,2017,638,1513,851] == lst 
  end

  test "solve part I" do
    got = Spinlock.step_n(Spinlock.init, 328, 2017)
    lst = Enum.drop_while(got.buffer, fn(x) -> x != 2017 end) |> Enum.take(2)
    assert [2017, 1670] == lst 
  end
end
