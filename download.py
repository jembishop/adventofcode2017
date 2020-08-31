#!/usr/bin/python3.8
from aocd.models import Puzzle
import sys

day = int(sys.argv[1])
input = Puzzle(year=2017, day=day).input_data
open(f"inputs/{day}.txt", "w").write(input)
