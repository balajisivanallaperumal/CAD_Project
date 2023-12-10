
# Project Title: Pipelined Double precision (fp64)      Floating point Multiplier

* Bishal Kahar- CS22M032

* Balaji O S P- CS23Z060

## Project Structure

### Double Precision Floating Point

A structure called `FloatingPoint` is implemented to serve as a template for double precision floating point values. It includes the following components:

- **Sign Value:**
  - The sign value is stored as a Boolean value.

- **Exponent Value:**
  - The exponent value is stored as an e-bit value, where e is taken as a structure's input parameter.

- **Mantissa Value:**
  - The mantissa value is stored as an m-bit value, where m is taken as a structure's input parameter.

### Sign Manipulation

The sign values (Boolean) of both input operands are retrieved and compared. Based on the comparison, the sign of the final result is determined using the XOR logic operation. The final sign bit is then stored into the Sign Output of the `FloatingPoint` output variable.

### Exponent Manipulation

The exponent bits of both input operands are retrieved. The final adjusted exponent is calculated by adding the values and subtracting the bias value (1023). While performing Exponent Manipulation, the multiply rule also checks for potential infinite (positive or negative) and NaN (Not a Number when either input is Infinite) conditions.

### Mantissa Manipulation

The mantissa bits of both input operands are retrieved. The actual multiplication operation is performed on the aligned mantissa (after adding the hidden significant bit) bits using the `*` operator in Bluespec. The resulting mantissa bits are then enqueued into the Mantissa Output.

### Output

The processed and manipulated sign, exponent, and mantissa bits are stored in their respective `FloatingPoint` output variable.

### Normalization Process

The exponent and mantissa bits obtained from the multiply rule are received as inputs in the normalize rule. The mantissa is analyzed to check if its most significant bit (MSB) is 1. If the MSB is 1, indicating a non-normalized representation, the following steps are performed:

- The mantissa is shifted one bit to the right (mantissa[104:53] is considered).
- The exponent is incremented by 1 to maintain the correct value of the result.

Otherwise, the value of the exponent is unchanged, and the normalized mantissa (mantissa[103:52] is considered) is obtained. The normalized mantissa and the adjusted exponent are then enqueued in their respective output FIFOs.

### Output Delivery

The normalized mantissa and adjusted exponent are stored in their respective output FIFOs.
  
## Design Decisions

1. **Modularity and Scalability**: The code is designed in a modular fashion, with separate definitions for types, functions, and instances. This approach enhances readability, maintainability, and reusability.

2. **Floating Point Representation**: The code defines a `FloatingPoint` type with parameters for exponent and significand sizes. This flexible representation allows for various floating-point formats (like half, single, double precision) to be implemented using the same underlying structure.

3. **Exception Handling**: Considering the intricacies of floating-point arithmetic, the code robustly handles exceptions (like NaNs, infinity, underflow, overflow) which are critical for reliable numerical computations.

4. **Type Classes and Instances**: Implementing instances for standard type classes (like `Eq`, `Ord`, `Literal`) ensures that floating-point types work seamlessly with BSV's built-in operations and functions.


## Algorithms Used

1. **Floating-Point Arithmetic**: Key operations like addition, subtraction, multiplication, division, and comparison are likely implemented considering IEEE 754 standards, although the specific details depend on the rest of the codebase.

2. **Normalization and Rounding**: The code includes functions for normalizing and rounding floating-point numbers. These are essential for maintaining precision and handling the finite representation of real numbers.

3. **Special Number Handling**: Algorithms to handle special numbers like NaN (Not a Number), infinity, zero, etc., are implemented, adhering to the typical floating-point standards.

## Test Benches

1. **Multiplication of Two Positive Numbers (tb_float_pos_pos.bsv):**

   This test bench specifically focuses on verifying the behavior of the output of the multiplier whenever two positive floating-point numbers are multiplied.

2. **Multiplication of a Positive Number and a Negative Number (tb_float_pos_neg.bsv):**

   This test bench specifically focuses on verifying the behavior of the output of the multiplier whenever a positive floating-point number and a negative floating-point number are multiplied.

3. **Multiplication of Two Negative Numbers (tb_float_neg_neg.bsv):**

   This test bench specifically focuses on verifying the behavior of the output of the multiplier whenever two negative floating-point numbers are multiplied.

These test benches provide comprehensive coverage for our double precision floating-point multiplication unit.


## How to Run

1. It is assumed that the system has the Bluespec System Verilog compiler and the source code in the working directory.

2. Enter into the CAD_project directory:
   ```
   cd CAD_project 
   ```
3. Execute the following commands to clean, compile, link, and simulate the design:
   ```
   make clean compile link simulate
   ```
4. To test each test bench individually, modify the TOPFILE in the Makefile to point to the desired test bench file, and then repeat step 3. For example:
   ```
   TOPFILE = tb_float_pos_pos.bsv
    ```
    Additionally, you can generate the equivalent Verilog file for the BSV design using the command:
   ```
   make verilog
   ```
 5. After running the simulation, the VCD files can be found in the directory. You can view the simulation results using GTK Wave.

