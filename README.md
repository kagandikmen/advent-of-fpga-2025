# Advent of FPGA 2025 in Hardcaml

This repository contains my solutions to the [Advent of FPGA 2025](https://blog.janestreet.com/advent-of-fpga-challenge-2025/) challenge from Jane Street. It implements the solutions in Hardcaml, which is an "embedded hardware design domain specific language (DSL) implemented in OCaml" [[1]](https://arxiv.org/abs/2312.15035).

This repository is a fork of [hardcaml_arty](https://github.com/fyquah/hardcaml_arty) project, which is a Hardcaml library to interface with Arty A7 boards.

## Progress

|Day   |Status      |
|:-:   |:-:         |
|Day 1 |Completed   |
|Day 2 |Not started |
|Day 3 |Not started |
|Day 4 |Not started |
|Day 5 |Not started |
|Day 6 |Not started |
|Day 7 |Not started |
|Day 8 |Not started |
|Day 9 |Not started |
|Day 10|Not started |
|Day 11|Not started |
|Day 12|Not started |

## Solution Details

<details>
<summary><b>Day 1:</b> Door Password</summary><br>

[Solution](src/day01/) [Testbench](test/day01/)

##### Challenge Summary

The challenge of day 1 consists of two steps. For a given turning sequence, it needs to be computed:

- how many times the lock mechanism of a door stops at zero (Step 1)
- how many times the lock mechanism of a door hits zero (Step 2)

##### Solution

My solution takes a structured performance-first approach while avoiding the use of area- and power-hungry multiplication or division logic. The rotation values are first converted to 16-bit integers by the host, the sign of the integer depending on the direction of the rotation. These integers are then sent to the FPGA sequentially. After each integer arrives, the FPGA computes:

- whether the lock stops at zero (for step 1)
- whether the lock hits zero (for step 2)

These computations are performed while the next integer is still being transmitted. This allows for an efficient "pipelined" execution.

For each rotation, the logic computes the quotient and remainder of a division by 100 using iterative subtraction, taking advantage of the rotation values never exceeding 1000 in this case. The iteration count (defaulted to nine) can easily be increased or decreased for different rotation sequences. How the performance vs. area tradeoff would be affected by the usage of division operation may be inquired in the future.

</details>

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.

---