# Advent of FPGA 2025 in Hardcaml

This repository contains my solutions to the [Advent of FPGA 2025](https://blog.janestreet.com/advent-of-fpga-challenge-2025/) challenge from Jane Street. It implements the solutions in Hardcaml, which is an "embedded hardware design domain specific language (DSL) implemented in OCaml" [[1]](https://arxiv.org/abs/2312.15035).

This repository is a fork of [hardcaml_arty](https://github.com/fyquah/hardcaml_arty) project, which is a Hardcaml library to interface with Arty A7 boards.

## Advent Calendar (aka Project Progress)

██████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░&nbsp;&nbsp;&nbsp;16.7%

0️⃣1️⃣ ✅ &nbsp;&nbsp;&nbsp; 0️⃣2️⃣ ✅ &nbsp;&nbsp;&nbsp; 0️⃣3️⃣ ⬜ &nbsp;&nbsp;&nbsp; 0️⃣4️⃣ ⬜  
0️⃣5️⃣ ⬜ &nbsp;&nbsp;&nbsp; 0️⃣6️⃣ ⬜ &nbsp;&nbsp;&nbsp; 0️⃣7️⃣ ⬜ &nbsp;&nbsp;&nbsp; 0️⃣8️⃣ ⬜  
0️⃣9️⃣ ⬜ &nbsp;&nbsp;&nbsp; 1️⃣0️⃣ ⬜ &nbsp;&nbsp;&nbsp; 1️⃣1️⃣ ⬜ &nbsp;&nbsp;&nbsp; 1️⃣2️⃣ ⬜ 

## Solution Details

<details>
<summary><b>Day 1:</b> Secret Entrance</summary><br>

[Solution](src/day01/) [Testbench](test/day01/)

##### Challenge Summary

The challenge of day 1 consists of two steps. For a given turning sequence, it needs to be computed:

- how many times the lock mechanism of a door stops at zero (Step 1)
- how many times the lock mechanism of a door hits zero (Step 2)

##### My Solution

My solution takes a structured performance-first approach while avoiding the use of area- and power-hungry multiplication or division logic. The rotation values are first converted to 16-bit integers by the host, the sign of the integer depending on the direction of the rotation. These integers are then sent to the FPGA sequentially over UART. After each integer arrives, the FPGA computes:

- whether the lock stops at zero (for step 1)
- whether the lock hits zero (for step 2)

These computations are performed while the next integer is still being transmitted over UART. This allows for an efficient "pipelined" execution that overlaps communication and computation.

For each rotation, the logic computes the quotient and remainder of a division by 100 using iterative subtraction, taking advantage of the rotation values never exceeding 1000 in this case. The iteration count (defaulted to nine) can easily be increased or decreased for different rotation sequences. How the performance vs. area tradeoff would be affected by the usage of division operation may be inquired in the future.

</details>

<details>
<summary><b>Day 2:</b> Gift Shop</summary><br>

[Solution](src/day02/) [Testbench](test/day02/)

##### Challenge Summary

The challenge of day 2 also consists of two steps. For any given range, the hardware needs to compute how many integers are in the range with a digit sequence

- that is a subsequence repeating itself twice (Step 1)
- that is a subsequence repeating itself **at least** twice (Step 2)

For further reference, I called the integers that fulfill these criteria "silly numbers" and "goofy numbers", respectively. Don't ask why :D Note that silly numbers are always a subset of goofy numbers.

##### My Solution

In my solution, the range bounds are first converted into 40-bit integers. The host (testbench) then sends all values in the range sequentially through UART. After arriving at the FPGA, the values are then converted into binary-coded decimal (BCD) format to enable digit processing. The function that does the conversion takes advantage of the fact that the biggest range value has 10 digits, but this is easily configurable in the source code. The logic then computes how many leading zeros the number has. After that, it is computed whether the number is silly and/or goofy. Finally, the number is added to a running total or dismissed.

##### Suggestions

Instead of sending all values in the range one after another, an implementation that can iterate in the hardware would be more efficient. For this,

- an FSM
- logic to increment BCD

need to be added to the implementation.

</details>

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.

---