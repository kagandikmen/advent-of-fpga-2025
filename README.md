# Decorating the North Pole in Hardcaml

[![AoF 2025 - Puzzle Tests](https://github.com/kagandikmen/advent-of-fpga-2025/actions/workflows/run_tests.yaml/badge.svg)](https://github.com/kagandikmen/advent-of-fpga-2025/actions/workflows/run_tests.yaml)

This repository contains my solutions to the [Advent of FPGA 2025](https://blog.janestreet.com/advent-of-fpga-challenge-2025/) challenge from Jane Street.

This repository is a fork of the [Hardcaml Arty](https://github.com/fyquah/hardcaml_arty) project, which is a Hardcaml library to use Digilent Arty A7 boards with Hardcaml.

#### What is Hardcaml?

As its introductory [arXiv paper](https://arxiv.org/abs/2312.15035) puts it, Hardcaml is an embedded hardware design domain specific language. As its [GitHub README](https://github.com/janestreet/hardcaml) puts it, Hardcaml is an OCaml library for designing and testing hardware designs. In its core idea Hardcaml is very similar to Chisel. You can use Hardcaml to describe digital hardware in OCaml, run simulations, and convert your design into RTL (VHDL/Verilog).

## Contents

- [Advent Calendar (aka Project Progress)](#advent-calendar-aka-project-progress)
- [Project Structure](#project-structure)
- [Getting Started](#getting-started)
- [Building the Executables](#building-the-executables)
- [Running the Tests](#running-the-tests)
- [Programming your Arty A7 FPGA Board](#programming-your-arty-a7-fpga-board)
- [Solution Details](#solution-details)
- [Performance Summary](#performance-summary)
- [Resource Utilization Summary](#resource-utilization-summary)
- [License](#license)

## Advent Calendar (aka Project Progress)

██████████████████████████████░░░░░░&nbsp;&nbsp;&nbsp;83.3%

0️⃣1️⃣ ✅✅ &nbsp;&nbsp;&nbsp; 0️⃣2️⃣ ✅✅ &nbsp;&nbsp;&nbsp; 0️⃣3️⃣ ✅✅ &nbsp;&nbsp;&nbsp; 0️⃣4️⃣ ✅✅  
0️⃣5️⃣ ✅✅ &nbsp;&nbsp;&nbsp; 0️⃣6️⃣ ✅✅ &nbsp;&nbsp;&nbsp; 0️⃣7️⃣ ✅✅ &nbsp;&nbsp;&nbsp; 0️⃣8️⃣ ✅✅  
0️⃣9️⃣ ✅✅ &nbsp;&nbsp;&nbsp; 1️⃣0️⃣ ⬜⬜ &nbsp;&nbsp;&nbsp; 1️⃣1️⃣ ✅✅ &nbsp;&nbsp;&nbsp; 1️⃣2️⃣ ⬜⬜ 

## Project Structure

```text
.
├── fpga                    # FPGA setup (synthesis to bitstream generation)
├── lib
│   ├── hardcaml_aof        # Advent of FPGA solution library
│   ├── hardcaml_aof_test   # Advent of FPGA testbench library
│   └── hardcaml_arty       # Hardcaml Arty library (for Arty A7 w/ Hardcaml)
├── src
│   └── dayXX
│       ├── dayXX.ml        # Solution for the puzzle of the day
│       └── dayXX_arty.ml   # Arty top module & RTL generator of the day
└── test
    └── dayXX
        ├── input.txt       # Puzzle input of the day
        ├── ref.py          # Reference solution of the day
        └── test_dayXX.ml   # Testbench for the solution of the day
```

Besides, you can refer to `src/old` and `test/old` for older solutions and their testbenches, respectively. Keep in mind that these old designs are intended as an archive and therefore not actively maintained.

## Getting Started

Either to build or to run the tests, you must have OCaml and opam on your machine. Refer to [OCaml's official installation guide](https://ocaml.org/docs/installing-ocaml) to install them.

After the installation, run:

```bash
opam init
opam switch create hardcaml 4.13.1
opam switch hardcaml
eval $(opam env)
opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
opam install ocaml-lsp-server odoc ocamlformat utop
opam install hardcaml hardcaml_waveterm ppx_jane ppx_expect ppx_deriving_hardcaml
```

This will create an opam switch `hardcaml` in version 4.13.1, switch to it, and install required dependencies.

**IMPORTANT:**

I used OCaml 4.13.1 working through the puzzles. You may observe peculiarities if you use a different version. (The compiler complaining because `of_int` is used instead of `of_int_trunc` is a strong sign thereof, for example.) Please check you are on the right version by using:

```bash
opam switch show
```

<details>
<summary>Are you on a different version?</summary><br>

If you are using a different version of OCaml, run:

```bash
opam switch create hardcaml 4.13.1
opam switch hardcaml
eval $(opam env)
```

to create a new opam switch with the right version, and switch to it. You can also refer to the [Github Actions workflow configuration](.github/workflows/run_tests.yaml) of this repository for the exact specifics of the intended setup.

</details>

## Building the Executables

Given you have all prerequisites in place, run:

```bash
dune build
```

from the project root to build all executables at once. You can also use:

```bash
dune build src/dayXX/dayXX_arty.exe
```

to specify which executable(s) you want built. After it is built, you can use:

```bash
dune exec src/dayXX/dayXX_arty.exe
```

to run the executable of your choice.

## Running the Tests

Given you have all prerequisites in place, run:

```bash
dune runtest
```
from the project root to run all the tests at once. You can also use:

```bash
dune runtest test/dayXX test/dayXY
```
to specify which tests you want to run. For the waveform files generated by the testbenches, navigate to the `/tmp/` directory on your machine.

**IMPORTANT:**

Advent of Code puzzle inputs differ by user. The expected values embedded into the testbenches may (and most likely will) become wrong if you replace my puzzle inputs with yours. You can, however, update the expected values after running the reference Python solution if you wish to use your own personal puzzle input.

## Programming your Arty A7 FPGA Board

The directory `fpga/` contains all the necessary files for the FPGA deployment of the solutions from synthesis to bitstream generation. For this, you need to have Vivado installed on your machine. To download Vivado, please see AMD's [Downloads](https://www.xilinx.com/support/download.html) portal for Vivado Design Suite. You may need to agree to certain terms and conditions.

If you have Vivado installed, you can proceed with the next steps. To generate the bitstream for the solution of day XX, first navigate into `fpga/` and then run:

```bash
make outputs/hardcaml_arty_top.bit DAY=dayXX
```

from within. You will find various reports and the bitstream file placed in `fpga/outputs/`. If you do not set the `DAY` variable, it defaults to day 1. You can also set the `BOARD` variable from the command line to pick the Arty A7 board of your choice. The options are `arty-a7-100` (default) and `arty-a7-35`.

Additionally, if you have djtgcfg installed, you can use:

```bash
djtgcfg prog --file outputs/hardcaml_arty_top.bit -d Arty -i 0
```

to flash your FPGA from the command line.

## Solution Details

### General Principles

I had four main design objectives while solving the puzzles. Sorted by importance, they are:

1. **FPGA deployability:** *"Don't make your design a pie in the sky; keep IO & area realistic, use known transmission protocols."*
2. **No host-side preprocessing of the puzzle input:** *"Imagine you have no host-side control."*
3. **High performance while staying area-conscious:** *"Make it fast but don't use a 512-bit wide multiplier if you don't have to."*
4. **Scalability:** *"How good can it handle inputs of different lengths and dimensionalities?"*

In accordance with the first design objective, FPGA deployability, I use UART for host-FPGA data transmission. UART is a well-known protocol and UART buses can be found on even the cheapest of FPGA boards. Again, to satisfy the first objective, I synthesize my solutions for my target FPGA board (Arty A7-100T) and share the resource utilization both in daily solution details and in its dedicated [section](#resource-utilization-summary) in this README file.

To satisfy the second objective, zero host-side preprocessing, I parse the text input of the puzzle on the FPGA. The host does no preprocessing of the input, not even data type conversions. The input text is sent through the UART bus in ASCII encoding.

To satisfy the third objective, area-conscious performance optimization, I try to avoid "first store then process" type of architectures in my solutions. This type of architectures result in large storages and longer execution time, although they are sometimes dictated by the nature of the puzzle. On top of this, I introduce parallelism and pipelining wherever I can, as long as it does not violate the first two design objectives. For each daily solution, I share in its description how many clock cycles it takes to complete. A summary of daily performance data can also be found in its dedicated [section](#performance-summary) in this README file. To evaluate performance, two metrics are used: time to complete, transmission over execution.

- **Time to Complete:** This is how many clock cycles it takes for the execution to be fully completed.
- **Transmission over Execution:** Mathematically this equals to (time to fully transmit input text)/(time to complete). I introduce this metric because I am using a performance-wise costly transmission protocol for the sake of FPGA deployability. To compensate for this, I introduce pipelining and/or parallelism whenever I can. This metric shows how successful I am in that for the puzzle of the day. Values closer to 100% mean that my solution is already optimized to the point that the performance bottleneck is the host-FPGA communication.

To satisfy the fourth objective, scalability, I try to architect my solutions in a way which enables them to be flexible about the input length and dimensionality. This has some limits, of course, due to constraints dictated by the very nature of hardware design. We cannot have infinite memory, or the input should not overflow result registers, for example.

Going through the solutions, you may occasionally see a lower-priority objective relaxed in favor of a higher-priority one. For example, UART is a relatively slow data transmission protocol. Nonetheless, I kept the UART transmission to keep my designs easily deployable on a cheap FPGA board. Another example would be day 5, where there is minimal host-side preprocessing to packetize the ASCII character stream of the input text. This was to simplify the complicated input parsing logic for that day. Considering a lot of transmission protocols work on the principle of packages anyway, I don't consider this to be an unrealistic exercise.

Additionally, in my solutions I "expect" the host to send the ASCII control character for "start of text" (STX, 0x02) before sending the puzzle input text. Likewise, I expect it to send "end of text" (ETX, 0x03) right after. However, this is fairly innocent input framing in my opinion, which entails no host-side processing.

Finally, solutions for day 8 and day 11 are validated using reduced inputs due to simulation runtime.

Below are the implementation details for the daily puzzles I worked on. They are all split into these four subsections: Summary, My Solution, Evaluation, Suggestions. In "Summary", I share a short summary of the puzzle as a reminder for the informed reader and an introduction for the innocent bystander. In "My Solution", I elaborate into how I implemented my solution. I usually explain my solution based on its control logic. It is also this subsection where I share information about any older solution I might have for the day and why I decided to refactor it. After that, I evaluate my solution against my main design objectives in subsection "Evaluation." Finally, in "Suggestions" I mostly share my observations and how you could improve on my solution.

<details>
<summary><b>Day 1:</b> Secret Entrance</summary><br>

<h2>Day 1: Secret Entrance</h2>

### Summary

The puzzle of day 1 consists of two parts. For a given turning sequence, it needs to be computed:

- how many times the lock mechanism of a door stops at zero (Part 1)
- how many times the lock mechanism of a door hits zero (Part 2)

The key detail is that it is a circular lock that is being turned. So the counter wraps at 100. (0 and 99 are neighbors.)

### My Solution

The state machines in [my solution](src/day01/day01.ml) are fairly simple. You can find them below. `States` signifies the "main states," which relate to general control of the execution. `Compute_states` signifies the execution steps of the "knob turner" logic that starts running in the background as input text starts flowing to the FPGA through the UART bus.

```ocaml
module States = struct
  type t = 
    | Idle
    | Receive
    | Compute
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module Compute_states = struct
  type t =
    | Idle
    | Turn
  [@@deriving sexp_of, compare, enumerate]
end
```

The FPGA starts at `Idle`. In this state it is doing nothing, except for the fact that it is listening to the UART bus for the special ASCII control character "start of text." Once it is received, the FPGA transfers to the state `Receive` where it starts receiving unprocessed raw input text through the UART bus. The logic parses every line, registering the R or L at the start as positive or negative turn, and at each line break it saves the received turn value into its 16-bit wide FIFO. When the ASCII control character for "end of text" is received, the logic moves onto the state `Compute` where it waits for the knob turner logic to complete. When it completes, the main FSM concludes everything by moving onto the state `Done`. 

The knob turner logic is another state machine (a very simple one) that "runs in the background" while the FPGA is actually still listening to the UART bus for new knob turn data. Knob turner logic is triggered when the FIFO is no longer empty. This causes the knob turner state machine to move to its only active state `Turn`. In this state the turn magnitude and direction are analyzed and the running totals for part 1 and part 2 of the puzzle are updated accordingly. Because this logic is fast enough to conclude before the next turn is saved into the FIFO, the FIFO depth requirement is extremely low for this application.

In my [older solution](src/old/day01/day01.ml) for this puzzle, I had a stateless design which required the values to be converted into 16-bit integers on the host side. As I later went on a newfound quest toward zero host-side processing, I felt the necessity to move away from this old design. To my surprise, the new design is 28% faster!

### Evaluation

**FPGA deployability:** ✅  
**No host-side preprocessing:** ✅  
**Scalability:** Can handle an infinitely long turn sequence. Turn magnitudes can go up to 999. This value can be changed by modifying the source code, but increasing it costs performance.  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                   797,505 |                99.998% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|         48 |    42 |      0 |      0 |   0 |

### Suggestions

The knob turner logic is both much faster than the UART transmission and runs in parallel to it. Therefore the UART transmission is the overwhelmingly biggest performance bottleneck. As one of my main design concerns is real-world FPGA deployability, I kept the UART bus. However, if you don't feel bounded by this, feel free to attack the data transmission first. You may need to increase the FIFO depth as you move towards faster transmission protocols. 

To avoid costly division logic, I used a 10-step subtraction routine instead of dividing the rotation magnitude by 100. The same routine, the function `divmod100`, also serves as modulo. It uses a trick, however: It makes use of the fact that our turns in the puzzle input never exceed 999. In case you have bigger turn magnitudes, you need to increase the number of subtraction steps by modifying `divmod100`.

<br><br><br></details>

<details>
<summary><b>Day 2:</b> Gift Shop</summary><br>

<h2>Day 2: Gift Shop</h2>

### Summary

The puzzle of day 2 also consists of two parts. For any given range, the hardware needs to compute how many integers are in the range with a digit sequence

- that is a subsequence repeating itself twice (Part 1)
- that is a subsequence repeating itself **at least** twice (Part 2)

### My Solution

**IMPORTANT:** For further reference, I called the integers that fulfill the part 1 criteria "silly numbers." Likewise, an integer is a "goofy number" if it fulfills the criteria for part 2. Don't ask why I did this :D

The state machines in [my solution](src/day02/day02.ml) are as below. `States` is the "main state machine" whereas `Compute_states` are the states of the "processing engine." Main state machine is responsible for receiving input and general execution control, whereas the compute states govern the timing with which the processing engine goes through the bounds.

```ocaml
module States = struct
  type t =
    | Idle
    | Receive
    | Compute
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module Compute_states = struct
  type t =
    | Idle
    | Read
    | Init
    | Evaluate
    | Next
  [@@deriving sexp_of, compare, enumerate]
end
```

At program start, the FPGA is in state `Idle` where it listens to the UART bus for the ASCII control character "start of text." After sending this character, the host sends the puzzle input text with no preprocessing. The flowing characters are registered as valid input data on the FPGA side, which is now in `Receive` state. The FPGA parses the incoming characters and saves the received range bounds into two FIFOs, one for lower bounds and one for upper bounds. The values are converted into binary coded decimal (BCD) using the double dabble algorithm before being saved. When the host sends the character "end of text," the FPGA moves onto the state `Compute` where it waits for the processing engine to complete bound processing. Once the processing engine is back in idle state, the main state machine concludes everything by moving further to its state `Done`.

The moving of the processing engine out of `Idle` is triggered by the arrival of first bounds in the FIFOs. The biggest advantage here is that the processing engines don't need to wait for full input arrival. Once the first bounds are received, they can start computing while the other bounds are still being transmitted.

The processing engine does not check every single integer in the range to figure out if it is silly and/or goofy. That is sometimes billions of integers to check. Therefore, the processing engine comes from the other side. It goes through all silly/goofy numbers and checks if they are in the given integer range. This is done in state `Evaluate` of the processing engine, and it has proven to be the faster approach. The other states of the processing engine are all transitional or related to bookkeeping.

In my [older solution](src/old/day02/day02.ml) for this puzzle, I was:

- turning range bounds into 40 bit integers on the host side,
- iterating over all the integers in the range on the host side, sending every single one through UART,
- computing on the FPGA (no state machine) if the latest-received integer is silly/goofy.

Horrible solution, I know. The new one is incomparably better.

### Evaluation

**FPGA deployability:** ✅  
**No host-side preprocessing:** ✅  
**Scalability:** Can handle an infinite number of integer ranges, as long as 64-bit result registers are not overflowed. Integers can have up to 10 digits.  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                 3,849,453 |                 5.784% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|        207 |    98 |      0 |      0 |   0 |

### Suggestions

The UART transmission continues running in parallel while the first ranges are already being processed. Considering this with the fact that processing one range takes much longer than just receiving it, the UART transmission is not the performance bottleneck in this application. The by far biggest chunk of execution is the evaluation state of the processing engine. 

Currently, the FPGA dynamically computes all silly/goofy numbers on the run. I am thinking a possible next step could be to explore what happens if we made this computation static. This is certain to increase performance, but I don't know what kind of effect it would have on area/resource usage. I also don't know if this increase in logic footprint would result in a performance increase good enough to justify the switch. You can try this out and let me know; don't hesitate to create an issue or pull request.

<br><br><br></details>

<details>
<summary><b>Day 3:</b> Lobby</summary><br>

<h2>Day 3: Lobby</h2>

### Summary

The puzzle of day 3 also consists of two parts. For any given digit sequence, the puzzle requires us to compute what is the highest achievable value after deleting a certain number of digits. In the first part, 2 digits are left; in the second part, 12 are left.

### My Solution

**IMPORTANT:** For further reference, I named the number of digits to leave "k." So, for the first part, k is equal to 2; for the second part, k is equal to 12.

[My solution](src/day03/day03.ml) for day 3 is a stateless one. In this stateless solution, the raw text input received through the UART bus is processed as digits arrive one by one. In this puzzle, each sequence has exactly 100 digits. This means we are allowed to drop 98 of them for the first part of the puzzle. 88 of them for the second part, likewise. For any given k, the FPGA first computes how many digits can be dropped per bank, 100 - k. Then it processes every arriving digit immediately by comparing it to the already-picked values. Given there are still enough remaining "drop credits" at the time of arrival, the previously-picked digits are dropped if they are smaller than the incoming one. Finally, once all 100 digits are processed, the remaining digits are added to a running total.

### Evaluation

**FPGA deployability:** ✅  
**No host-side preprocessing:** ✅  
**Scalability:** Can handle an infinite number of banks. Bank width, 100, is hardcoded. (See suggestions below.)  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                   888,846 |                   100% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|         30 |    29 |      0 |      0 |   0 |

### Suggestions

My implementation hardcodes the bank width, 100, into the logic. Although this is easily changeable in the source code, one may want a design capable of adjusting itself to different bank widths. This could be managed by first saving the bank(s) in the FPGA and then processing the digits one by one, but this would both require storage and reduce performance. In the most ideal case, the digit sequences are processed "on the go" as they arrive without introduction of hardcoded widths, arrays, RAMs, FIFOs, or state machines. I considered all these additional components, and decided for this case that hardcoding the bank width was the least harmful. (Just look above for the incredible metrics! I didn't want to ruin that.)

If you can come up with a solution including none of those; don't hesitate to create an issue or pull request.

Nevertheless, the number of digits to leave, k, is not hardcoded. It is a parameter of the `create` function. So, my solution is actually in no way tailored to the first or second part of the puzzle. If you wonder what the result would be for k = 3, just instantiate my solution with k set to three!

<br><br><br></details>

<details>
<summary><b>Day 4:</b> Printing Department</summary><br>

<h2>Day 4: Printing Department</h2>

### Summary

The puzzle of day 4 requires us to solve a k-core peeling algorithm; 4-core in this case. The first part asks for how many vertices are removed in the first iteration, whereas the second part asks for how many are removed for an iteration count approaching infinity.

### My Solution

[My solution](src/day04/day04.ml) implements an iterative, multi-pass, memory-resident algorithm with the following finite state machine:

- **IDLE**: In this initial state, the FPGA does nothing other than listening to the UART bus for the trigger signal, the ASCII control character for "start of text." When it arrives, the FSM moves onto the state LOAD.
- **LOAD**: In this state, all field info is loaded onto the FPGA. The puzzle input is transmitted by the host through the UART bus with zero preprocessing. If the FPGA registers the inflowing 8-bit value as a valid ASCII character, it stores it in a 256x256 grid storage. Each "cell" of the grid has the following structure:

```ocaml
module Cell = struct
  type 'a t =
  {
    is_roll: 'a;
    removed_this_pass: 'a;
  }

  ...
end
```

Both `is_roll` and `removed_this_pass` are 1-bit wide. The state LOAD only writes `is_roll` though, `removed_this_pass` is modified in the state REMOVE. In LOAD, the FPGA also figures out the dimensionality of the input, which means it detects how many rows and columns of characters there are in the input text. After the loading of all input fields is complete, the FSM continues with the other states.

- **REMOVE**: This state iterates over all cells, and sets their `is_roll` field zero if they are both occupied by a roll and can be accessed according to the rules of the puzzle. (This is essentially "removing the paper roll.") Once the iteration is done, the logic checks if `max_passes` number of remove passes is achieved. If so, then the FSM moves onto DONE. If not, then it goes to SWITCH for the evaluation of the current remove pass and the transition to the next one. The number of removed rolls is accumulated in the register `total_removed`.

- **SWITCH**: This state takes up only one cycle and acts as a "bookkeeping step" after each remove pass is completed. After this one cycle, the FSM returns back to REMOVE for the next remove pass if the required number of passes is not yet achieved.

- **DONE**: This is the state the logic arrives at after breaking from the REMOVE-SWITCH loop. The FPGA signals back to the host that the computation is done, so the host knows that `total_removed` is stabilized.

If `max_passes` is set to zero, the remove passes continue forever until there are no removable rolls left. For the first part of the puzzle, `max_passes` is set to one. For the second part, it is set to zero.

In my [older solution](src/old/day04/day04.ml) for this puzzle, I was:

- first saving all the values into the grid likewise (LOAD)
- then going through all the cells of the grid to mark the rolls 'accessible' or not (MARK)
- then going through all the cells once again to remove the rolls marked accessible (REMOVE)

Because this involved iterating twice over the grid for each remove pass, it was very inefficient and significantly slower. After moving to this new solution I have seen a 9.4% acceleration for the first part of the puzzle and a 36.5% acceleration for the second part.

I have another retired solution for this puzzle which was similar to the current one but it had the puzzle input dimensionality, 140, hardcoded into the hardware. Current version can now accommodate row counts and column counts up to 254, and they can also be different from each other. The hardware picks up the dimensions automatically using the positioning of the newline characters.

### Evaluation

**FPGA deployability:** ✅  
**No host-side preprocessing:** ✅  
**Scalability:** Can handle 2D spaces up to 254x254. Row count and column count can be different.  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                 1,910,574 |                44.178% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|        111 |    79 |      0 |      0 |   0 |


### Suggestions

Because we are instructed in part 2 to peel infinitely until there is no more peeling to do, the grid has to be stored on the FPGA. There seems to be no getting rid of this; the storage is a necessity rather than a design choice. The control logic and the state machine also seem lean enough to me. Each cell of the grid is 2-bit wide, so my solution does not have an excessive memory/area footprint, either.

The waveforms reveal that the biggest performance bottleneck of the application is the UART transmission, but I intend to keep the UART bus as FPGA deployability is my biggest design objective. Nevertheless, if you are bound by different design objectives and constraints, the host-FPGA data transmission is the part of the solution you should look at first.

What I especially like about this solution is that it is actually in no way tailored to part 1 or 2 of the puzzle. `max_passes` is a design parameter that goes into the `create` function. If you are curious about how many rolls would be removed after three remove passes, just set `max_passes` three while you instantiate my solution!

<br><br><br></details>

<details>
<summary><b>Day 5:</b> Cafeteria</summary><br>

<h2>Day 5: Cafeteria</h2>

### Summary

Like the others, the puzzle for day 5 consists of two parts. It is based on a text input that consists of lines that are either integers (IDs) or integer ranges. In the first part, it is computed how many of the IDs fall on at least one of the ranges. In the second part, it is computed how many integers in general fall on at least one of the ranges. The main challenge, especially in part two, is that the ranges both overlap and come unsorted.

### My Solution

[My solution](src/day05/day05.ml) implements a data structure called "Package" to standardize sending of ranges, ingredient IDs, and other sorts of control signals. Every package consists of an 8-bit flag and 64-bit payload. The 8-bit flag:

- `0x01` signals that the payload is the lower bound of a new range,
- `0x02` signals that the payload is the upper bound of the range the lower bound of which was just sent,
- `0x03` signals "section change," which means that all ranges are done being sent and ingredient IDs will follow,
- `0x04` signals that the payload is an ingredient ID,
- `0xFF` signals EOF.

Other 8-bit values are invalid as flags. My solution then implements the following state machine:

```ocaml
module States = struct
  type t =
    | Read_ranges
    | Read_ids
    | Scan
    | Merge
    | Count
    | Done
  [@@deriving sexp_of, compare, enumerate]
end
```

The FPGA, starting in the state `Read_ranges`, first reads all the ranges in the order they are fed to the UART bus by the host. The moment the host signals a section change, the logic transfers to the state `Read_ids`. When the host is done with sending all IDs, it signals EOF, and the logic starts sorting the ranges. The sorting process (selection sort) starts with the state `Scan`. In this state, the FPGA looks for the range with the lowest lower bound that is not marked as "used" yet. Then it moves onto the state `Merge`, which is where we scan through all unused ranges once again for candidates eligible for a range merge. Then, if there are still unused ranges left, the FPGA goes back to the state `Scan`. Once the `Scan`-`Merge` loop is completed, the logic goes into the `Count` state. In this state, we count how many of the IDs fall in any one of the merged ranges. Finally, the FPGA arrives at the state `Done`, where it signals back to the host that the computation is successfully completed.

### Evaluation

**FPGA deployability:** ✅  
**No host-side preprocessing:** ⚠️ (packetization on the host-side)  
**Scalability:** The input text can include up to 200 ranges. Likewise, it can include up to 1024 IDs. These values can be increased by modifying the source code, but this comes with a performance & area cost.  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                   585,362 |                94.139% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|      10567 | 13316 |      0 |      0 |   0 |

### Suggestions

One suggestion could be made about the packetization, in case we are on a quest to save every single cycle possible. The section change and EOF signals do not send any meaningful payload, but the FPGA waits for this payload to be fully sent before going forward with the signaled operation. In current implementation, the host fills the payload field with zeros. This is not even remotely the performance bottleneck of the application, but fixing it would save a couple cycles.

<br><br><br></details>

<details>
<summary><b>Day 6:</b> Trash Compactor</summary><br>

<h2>Day 6: Trash Compactor</h2>

### Summary

Like the others, the puzzle for day 6 consists of two parts. It is based on a text input that consists of a matrix of integers, followed by a final row of operation signs. (Either addition or multiplication in this case.) The integers (and the operator) that are found in the same vertical stack belong to each other and constitute an operation. In part 1, the integers are to be parsed left to right, whereas in part 2 they are parsed from top to bottom.

### My Solution

**IMPORTANT:** For further reference, I named this union of integers and operator in the same stack an "opblock." In the AoC puzzle input, each opblock consists of three integer values and an operator.

[My solution](src/day06/day06.ml) starts its life in `Idle` state (The full state machine is below.) and immediately starts listening to the UART bus. When the host sends the ASCII value for "start of text," the FPGA transfers into the state `Receive`. In this state, the FPGA saves the characters of the incoming text (with zero host-side preprocessing) one by one in its RAM.

```ocaml
module States = struct
  type t =
    | Idle
    | Receive
    | Find_opblk_start
    | Find_opblk_end
    | Setup_compute
    | Compute
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module Secondary_states = struct
  type t =
    | Idle
    | Retrieve
    | Accumulate
    | Done
  [@@deriving sexp_of, compare, enumerate]
end
```

When the host transfers the ASCII value "end of text," the FPGA moves onto the state `Find_opblk_start`. Combined with the state `Find_opblk_end`, the FPGA iterates over all columns to figure out at which columns the next opblock starts and ends. (Don't confuse columns with stacks or opblocks, columns are one character wide. Think of the input text as a matrix of ASCII characters.) Once the opblock borders are figured out, the FPGA continues with the states `Setup_compute` and `Compute`, the latter of which activates the "secondary state machines." These secondary state machines are another type of state machine, which we instantiate twice. They work simultaneously; and they only differ in the specifics of the computation one of their states includes. The first state machine computes for part 1, and the second computes for part 2. So, for each opblock, the parts 1 and 2 are computed in parallel, saving latency.

When both secondary state machines arrive at their `Done` state and return, the primary state machine adds the return values into a running total and continues with the next iteration of the loop: `Find_opblk_start`-`Find_opblk_end`-`Setup_compute`-`Compute`. After all opblocks are processed this way, the running totals are returned to the host.

There was an [older solution](src/old/day06/day06.ml) for this puzzle where the characters were saved in an array called `grid` instead of a RAM. This older array-based implementation had an exceedingly high simulation runtime, simulating the newer RAM-based implementation is incomparably faster. With the full puzzle input, the newer solution takes ~4 minutes to simulate on my machine. The older solution was still going strong when I terminated it after an hour. Nevertheless, they are equivalent in terms of FPGA performance.

### Evaluation

**FPGA deployability:** ✅  
**No host-side preprocessing:** ✅  
**Scalability:** The input text can have up to 8 rows, and each row can have up to 4096 characters. These values can be increased by modifying the source code, but this comes with a performance & area cost.  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                   848,896 |                97.081% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|       8411 |  4273 |      0 |      0 |   0 |

### Suggestions

I feel like the main state machine could be made more compact by removing the state `Setup_compute`. I am planning to revisit this puzzle to see if the compute setup can be moved to the `Find_opblk_end` and/or `Compute` states.

Another idea would be to implement "opblock engines" for each opblock received/saved. Iterating over the rows (either during transmission or after it,) each value on the row would be sent to its dedicated opblock engine, which would both add and multiply all the operands as they come. Once the final row arrives with the correct operation, the opblock engine would then already have the result and return it immediately.

But all this under a condition: As the relation "opblocks per row" grows, this idea of parallel processing would prove impractical due to power and/or area concerns. A very big opblock/row ratio was indeed the case for me, so I didn't go that direction. But feel free to consider this idea if you have a different input dimensionality.

The biggest performance bottleneck of the new version of the application is the UART transmission, which consumes around 97% of the cycles. I intend to keep the UART bus for seamless FPGA deployment in the future, which is one of my four main design objectives. In case your situation allows you to opt for a parallel (or simply faster) protocol, this opblock engines idea would be worth considering.

<br><br><br></details>

<details>
<summary><b>Day 7:</b> Laboratories</summary><br>

<h2>Day 7: Laboratories</h2>

### Summary

The part 1 of the seventh puzzle requires us to find how many times a beam split event happens in a given positioning of beam inputs and splitters in a 2D space. The second part requires the computation of how many alternative paths there are for a beam to follow from the beginning (top) until the end (bottom.)

### My Solution

The state machines in [my solution](src/day07/day07.ml) are as follows:

```ocaml
module States = struct
  type t =
    | Idle
    | Receive
    | Compute
    | Conclude
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module Compute_states = struct
  type t =
    | Idle
    | Compute_row
  [@@deriving sexp_of, compare, enumerate]
end
```

In terms of general control logic, this solution is similar to what you will find in solutions for day 1 and 2. `States` lists the states of the main state machine. `Compute_states` are the states of the "compute engine" for the row processing logic which runs in parallel.

The main state machine starts at state `Idle` and listens to the UART bus for the ASCII control character "start of text." When it is received, the main state machine transfers to the state `Receive` where it starts registering UART input as valid puzzle data. At each line break, it saves the received row data into a FIFO, one bit for each character. (Zero for space, one for splitter.) This FIFO is 256 bits wide, so the design can work with column counts less than or equal to 256. The FPGA continues receiving and saving until the control character for "end of text" is received. Then, it transfers to the state `Compute` where it waits for the compute engine to complete. After that, it concludes the entire computation (`Conclude`) and finally finishes. (`Done`)

The compute engine, which has the simple state machine `Idle` <-> `Compute_row`, fulfills the function of reading latest row from the FIFO, analyzing it, and updating various registers to keep track of the current state of the computation. It sits idle while the FIFO is empty, whereas it transfers to `Compute_row` when there is any row data in the FIFO.

This approach with two state machines enables a great deal of parallelism. The rows are processed as they come; they are not first saved and then processed in bulk, which would cost a lot of performance and area. Moreover, the compute engine processes all columns of the row in parallel (by taking account of what happens in the neighbor columns), and therefore processing an entire row takes only one cycle!

In fact, I had an [older solution](src/old/day07/day07.ml) for this puzzle where I used to:

- first save all the puzzle input into a 142x142 grid,
- wait for all of the input to be fully received and saved,
- then iterate over all the rows and columns one cell at a time.

This was of course neither an elegant solution nor efficient. The performance comparison of the older and newer solutions are as follows:

|           | Time to Complete (cycles) | Transmission/Execution |
|-----------| ------------------------: | ---------------------: |
| **New**   |                   887,266 |                99.998% |
| **Old**   |                   907,426 |                97.778% |

Because the total execution is dominated by the UART transmission, the total time is reduced by 2% only. However, the time spent after the UART transmission is reduced by 99.5% with the newer solution.

### Evaluation

**FPGA deployability:** ✅  
**No host-side preprocessing:** ✅  
**Scalability:** Can handle an infinite number of rows. Column count can go up to 256. This can be increased by modifying the source code, but with a performance & area cost.  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                   887,266 |                99.998% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|         61 |    44 |      0 |      0 |   0 |

### Suggestions

The performance metrics display very well what the main performance bottleneck of the application is: the UART transmission. I will keep the UART bus because one of my main design objectives is FPGA deployability, but feel free to attack the data transmission if you are bound by different concerns and constraints. You may need to adjust the FIFO depth in case your transmission bandwidth is higher.

One of the great things about this solution is that it actually does not put any limit on how many rows the input text is allowed to have. The AoC puzzle input has 142 rows like it has 142 columns, but the Hardcaml solution is able to continue until its 64-bit `cur_rays` registers or result registers are overflowed.

<br><br><br></details>

<details>
<summary><b>Day 8:</b> Playground</summary><br>

<h2>Day 8: Playground</h2>

### Summary

For the puzzle of day 8 we have points in 3D space. And these points need to be connected starting from the shortest-distanced pair. Connected points constitute a "circuit." The first part of the puzzle asks for the sizes of three biggest circuits after a certain number of connections. The second part asks us to connect until all points are connected (directly or indirectly) and then asks for the x-coordinates of the points of the latest-connected pair.

### My Solution

[My solution](src/day08/day08.ml) first receives the text input through the UART connection without any host-side preprocessing. It then goes over all possible connections to compute the distances between point pairs. I used squared distances in my solution to spare costly square root logic. Because we are interested in the order of the distances only, not the real values themselves, working with squared distances does not affect correctness in this case.

After all distances are computed, the FPGA sorts the connections by ascending distance. For this, I used bitonic sort because of its somewhat hardware-friendly nature. 

The logic then continues by initiating the "graph," which is just two arrays in this case. The idea is similar to what I implemented in [my Python reference solution](test/day08/ref.py), which is basically [Kruskal's algorithm](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm). In this application of the algorithm, we treat the points as the vertices of a graph, and the connections between them as the edges. We use two arrays (lists in Python) both with a size equal to the number of points. The first array is called `parents` and stores the parent of each vertex. For each circuit one of the vertices is the "eldest parent" or "root," which means it directly or indirectly fathers all the other vertices in the circuit. (It actually does not matter much which vertex the root is.) The second array is called `sizes` and for each root vertex it stores the size of its circuit. For vertices that are not the root of the circuit, the size array does not store any meaningful value.

After the bitonic sort, the FPGA continues by setting the graph up, and then it starts fetching the possible connections one by one, starting from the shortest-distance one. If the vertices are not already in the same circuit, their circuits are merged. Once all vertices are part of the same circuit, the FPGA concludes the computation. As usual, the host is notified for the end of computation through a done signal.

### Evaluation

*This solution was dimensioned for a reduced input. See Suggestions.*

**FPGA deployability:** ⚠️ (Reduced input version fits in target FPGA. Full input version does not.)  
**No host-side preprocessing:** ✅  
**Scalability:** Can handle up to 64 vertices. This can be increased by modifying the source code, but with a performance & area cost.  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                   420,368 |                 9.243% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|      51650 |  7990 |      8 |      0 |  12 |

### Suggestions

I have to admit that this was the most difficult puzzle so far; the task does not lend itself well to hardware. Sorting is extremely costly, which is why I even needed to reduce the input to 50 points.

Because part 1 asks for an intermediate state of the graph after a specific number of connections **starting from the shortest edge**, Kruskal's algorithm cannot be replaced by Prim's, Boruvka's, or reverse-delete in this case.

I observed that the main performance bottleneck of the application is the sorting. Therefore, you may want to attack sorting first if you ever work on improving my design.

<br><br><br></details>

<details>
<summary><b>Day 9:</b> Movie Theater</summary><br>

<h2>Day 9: Movie Theater</h2>

### Summary

For the puzzle of day 9 we work on a matrix of "tiles," which are either red, green, or another irrelevant color. The tiles are ordered in a way so that the red ones constitute a polygon when they are virtually connected with each other consecutively. This polygon is then, including its borders but not its corners, filled with green tiles. The first part of the puzzle asks for the area of the biggest rectangle with two red tiles as two of its opposite corners. The second part adds a criteria and asks for the area of the biggest rectangle with opposite red corners and a fully red-green surface.

### My Solution

Here is the state machine in [my solution](src/day09/day09.ml):

```ocaml
module States = struct
  type t =
    | Idle
    | Receive
    | Compute
    | Done
  [@@deriving sexp_of, compare, enumerate]
end
```

My design first receives raw text input through the UART bus (In alignment with the ASCII standard, `0x02` means "start of text", and `0x03` means "end of text.") The FPGA starts its execution in the `Idle` state and immediately starts listening for the start of text character. At the moment of its detection, the design transfers to the state `Receive` and starts saving received values into arrays for x and y coordinates. These coordinates of the red tiles are also saved into a double-port RAM. When the end of text character is received, the FPGA moves onto the state `Compute`. In this state, the FPGA iterates over all two-combinations of red tiles, which I call "corner pairs" for further reference. Each corner pair forms a virtual rectangle, and for each one of these virtual rectangles we compute:

- area,
- whether there is any border inside, formed by two consecutive red tiles.

The coordinates of the corner pairs are read from the arrays. For each corner pair, all red tiles need to be checked to see if any two consecutive tiles form a border edge that stays inside the virtual rectangle. For this border computation, the consecutive red tile pairs are read from the double-port RAM. (This is the reason behind the existence of both the arrays and the RAM. For each corner pair, we have to iterate through all the other tiles two at a time.)

When all scenarios are finally processed, the FPGA transfers to the `Done` state and concludes the computation. As usual, the host is notified about this using a done signal.

In my [older solution](src/old/day09/day09.ml), I used to:

- First receive the coordinates,
- Save them into the arrays,
- Then move onto a state named `Find_borders`, where I crawled between consecutive red tiles and saved the coordinates of every single border tile in between into a single-port RAM (which means ~70k coordinates,)
- Then for each corner pair, go through the entire RAM to see if any of the border tiles is inside the virtual rectangle.

This was orders of magnitude slower and more area-hungry than my current solution, of course. In fact, I wasn't even able to run the entire text input due to time concerns. For the same small toy input of 50 red tiles, I have seen the execution time drop from 62550 us to 360 us after moving to the newer solution, which constitutes a 99.4% acceleration.

### Evaluation

**FPGA deployability:** ✅  
**No host-side preprocessing:** ✅  
**Scalability:** Can handle up to 512 red tiles. This value can be increased by modifying the source code, but with a performance & area cost.  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                12,635,280 |                 2.001% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|      54973 | 33347 |      2 |      0 |   4 |

### Suggestions

After the recent changes, ~74% of the execution time goes to the UART transmission. As FPGA deployability is one of my main design goals, I didn't touch this. If you have other design objectives and constraints, consider attacking this big chunk of execution first.

<br><br><br></details>

<details>
<summary><b>Day 11:</b> Reactor</summary><br>

<h2>Day 11: Reactor</h2>

### Summary

The first part of puzzle 11 asks us to find the number of all possible paths from node `you` to `out` in a directed acyclic graph. The second part extends the criteria by asking the number of all paths from node `svr` to `out` under the condition that the path has to visit nodes `dac` and `fft` on its way, notwithstanding the order.

### My Solution

[My solution](src/day11/day11.ml) immediately starts listening to the UART bus for the ASCII control character "start of text." When it is finally received, the FPGA starts registering UART values as valid graph data and saves the data into a set of arrays that together serve as the adjacency matrix. When the character "end of text" is received, the FPGA accepts the graph as fully transmitted and starts processing received data to build the topological sorted order for the graph. For this, the logic uses Kahn's algorithm. Here is a nice [article](https://www.geeksforgeeks.org/dsa/topological-sorting-indegree-based-solution/) about topo sort and Kahn's.

The number of paths from source node A to target node B depends on the number of paths from "the children of A" to B. And the number of paths from the children of node A to node B depends on their children, as well. In software, this is usually implemented using recursion; but because we are in hardware, we process the topological order in reverse instead. Because no parent comes after any of its children in the topological order, crawling the order in reverse guarantees all children are fully processed before a parent node comes up. The number of paths for children are saved in an array for later consumption during the processing of their parent. Once the source node is finally arrived at, this computation is concluded.

I also used a trick. The second part of the puzzle asks for the number of paths from the source node `svr` to target node `out` with the condition the nodes `dac` and `fft` need to be visited along the path. In my reference Python solution, I used a mechanism for each child node to propagate up if these nodes have been seen, but in my Hardcaml solution I did something different: This second part of the puzzle can be reduced to the first part, because:

```text
NP(svr -> dac -> fft -> out) + NP(svr -> fft -> dac -> out) = (NP(svr -> dac) * NP(dac -> fft) * NP(fft -> out)) 
                                                            + (NP(svr -> fft) * NP(fft -> dac) * NP(dac -> out))

NP: number of paths
```

So, after the topological sorted order is complete, the FPGA does three passes over this order to compute:

```text
NP(you -> out)    # For Part 1
NP(svr -> dac)    # For Part 2
NP(svr -> fft)    # For Part 2
NP(dac -> fft)    # For Part 2
NP(fft -> dac)    # For Part 2
NP(dac -> out)    # For Part 2
NP(fft -> out)    # For Part 2
```

Finally, these values are used to compute the values required for both parts of the puzzle, and then the host is notified with a done signal.

### Evaluation

*This solution was dimensioned for a reduced input. See Suggestions.*

**FPGA deployability:** ⚠️ (Reduced input version fits in target FPGA. Full input version slightly exceeds the LUT count.)  
**No host-side preprocessing:** ✅  
**Scalability:** Can handle up to 256 nodes and up to 1024 edges. These values can be increased by modifying the source code, but this comes with a performance & area cost.  

#### Performance

| Time to Complete (cycles) | Transmission/Execution |
| ------------------------: | ---------------------: |
|                     5,158 |                94.707% |

#### Utilization

| Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| ---------: | ----: | -----: | -----: | --: |
|      45930 | 41995 |      0 |      0 |   0 |

### Suggestions

I am an electrical engineer; my exposure to concepts such as topological order or Kahn's algorithm does not come from a formal CS training. If you think this could be done better, either through a simpler/better implementation or through a completely different algorithm, please open an issue.

As simulating hardware is very slow, I once again switched real puzzle input with a much smaller toy input.

<br><br><br></details>

## Performance Summary

Below is a summary of the performance metrics of each day. These values are extracted from waveforms of the simulations.

| Day | Time to Complete (cycles) | Transmission/Execution |
| --- | ------------------------: | ---------------------: |
| 01  |                   797,505 |                99.998% |
| 02  |                 3,849,453 |                 5.784% |
| 03  |                   888,846 |                   100% |
| 04  |                 1,910,574 |                44.178% |
| 05  |                   585,362 |                94.139% |
| 06  |                   848,896 |                97.081% |
| 07  |                   887,266 |                99.998% |
| 08* |                   420,368 |                 9.243% |
| 09  |                12,635,280 |                 2.001% |
| 11* |                     5,158 |                94.707% |

\* *reduced input*

## Resource Utilization Summary

Below is a summary of the post-synthesis utilization results of each day for Digilent's Arty A7-100T board with XC7A100TCSG324-1 FPGA. To extract these metrics, the Vivado command `report_utilization -hierarchical -file outputs/post_synth_util_hier.rpt` was used.

| Day | Total LUTs |   FFs | RAMB36 | RAMB18 | DSP |
| --- | ---------: | ----: | -----: | -----: | --: |
| 01  |         48 |    42 |      0 |      0 |   0 |
| 02  |        207 |    98 |      0 |      0 |   0 |
| 03  |         30 |    29 |      0 |      0 |   0 |
| 04  |        111 |    79 |      0 |      0 |   0 |
| 05  |      10567 | 13316 |      0 |      0 |   0 |
| 06  |       8411 |  4273 |      0 |      0 |   0 |
| 07  |         61 |    44 |      0 |      0 |   0 |
| 08* |      51650 |  7990 |      8 |      0 |  12 |
| 09  |      54973 | 33347 |      2 |      0 |   4 |
| 11* |      45930 | 41995 |      0 |      0 |   0 |

\* *reduced input*

According to its [data sheet](https://docs.amd.com/v/u/en-US/ds180_7Series_Overview), XC7A100TCSG324-1 FPGA has the following features:

|    Device | Total LUTs |    FFs | RAMB36 | RAMB18 | DSP |
| --------- | ---------: | -----: | -----: | -----: | --: |
| XC7A100T  |     101440 | 126800 |    135 |    270 | 240 |

Apart from full input versions of days 8 and 11, all solutions comfortably fit on my target FPGA.

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.
