-- SerialDivider_16_20_bit_tb.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- Library for generating random numbers

--Testbench Name: 
-- 	SerialDivider_16_20_bit_tb
--
--Description: 
--	It simulates pressing the keypad to input values and initiate calculation.
--	It generates random dividends and divisors to test random general cases, and 
--	then test the edge cases.
--  Run the simulation for 4 seconds to complete all test cases.
--
--  **How the system works when in 16-bit operation mode (NUM_NIBBLES = 4)**
--	The hardware has fifteen 7-segment LEDs: five for dividend, five for divisor, 
--	five for quotient. However, if the system is in 16-bit division mode
--	(Set NUM_NIBBLES = 4) and you input more than 4 digits using the keypad, 
--	the input digits will shift left and the leftmost(top) digit will be lost.
--  Only the low 4 digits will be used for division and the leftmost LEDs will 
--	always display zero. 
--	
--  **Example of how the system works (for both 20-bit and 16-bit operation mode)**
--  Let's say you input FCAAA(dividend) / 075A4(divisor) = 00022(quotient) using 
--	the keypad.
--  If the system is in 20-bit division mode, the board will display
--	F C A A A (dividend)
--  0 7 5 A 4 (divisor)
--  0 0 0 2 2 (quotient)
--  However, if the system is in 16-bit mode, the leftmost digit will be lost when 
--	the keypad input shifts left. 
--	Thus, the division above becomes CAAA(dividend) / 75A4(divisor) = 0001(quotient)
--  The board will display
--  0 C A A A (dividend)
--  0 7 5 A 4 (divisor)
--  0 0 0 0 1 (quotient)
--
--  **TEST CASES**
--		a. 10 general cases of random dividend and divisor
--		b. 10 edge cases of random VERY LARGE dividend and VERY SMALL divisor pairs.
--		   	 VERY LARGE dividend's range: [1032768, 1048575] for 20-bit mode, [49728, 65535] for 16-bit mode	
--		 	 VERY SMALL divisor's range: [1, 6] for both 20-bit mode and 16-bit mode
--		c. Edge case of dividng by zero (xFFFFF/x00000). Outputs the error value(FFFFE for 20-bit, FFFE for 16-bit)
--		d. Edge case of dividng zero (x00000/xFFFFF). Outputs zero.
--
--  The testbench uses "uniform" function defined in "math_real" library with
--  for loops to generate random dividends and divisors. 
--
--  Revision History:
--		01/11/2019	Sung Hoon Choi	Created
--      01/13/2019	Sung Hoon Choi  Implemented random generation of test cases
--		01/15/2019	Sung Hoon Choi	Added comments

entity SerialDivider_16_20_bit_tb is
end SerialDivider_16_20_bit_tb;

architecture TB_ARCHITECTURE of SerialDivider_16_20_bit_tb is

    -- The component (SerialDivider_16_20_bit) to be instantiated and tested
	-- Port descriptions are included in the entity code
    component SerialDivider_16_20_bit
        generic(NUM_NIBBLES : integer := 5);
        port (
            nReset      :  in   std_logic;
            nCalculate  :  in   std_logic;
            Divisor     :  in   std_logic;
            KeypadRdy   :  in   std_logic;
            Keypad      :  in   std_logic_vector(3 downto 0);
            HexDigit    :  out  std_logic_vector(3 downto 0);
            DecoderEn   :  out  std_logic;
            DecoderBit  :  out  std_logic_vector(3 downto 0);
            CLK         :  in   std_logic
        );
    end component;
    
    -- Choose 20-bit divison mode or 16-bit division mode
	-- To test 20-bit, set NUM_NIBBLES = 5
    -- To test 16-bit, set NUM_NIBBLES = 4
     constant NUM_NIBBLES : integer := 4;


	-- CurDigit's values when resetting the KeypadRdy signal
    -- NOTE: Since we receive a new key input at CurDigit = 4 (for dividend) or  at CurDigit = 9 (for divisor), 
	--       it is ok to reset the KeypadRdy signal when CurDigit transitions to 3 (for dividend) or 8 (for divisor)
    constant RESET_DIVIDEND_KEYPADRDY_DIGIT : std_logic_vector(3 downto 0) := "0011";
    constant RESET_DIVISOR_KEYPADRDY_DIGIT : std_logic_vector(3 downto 0) := "1000";
    
	
	-- Values that indicate whether we are inputting a divisor or a dividend
    constant INPUT_DIVISOR: std_logic := '1';  -- input the divisor
    constant INPUT_DIVIDEND: std_logic := '0'; -- input the dividend
	
	
    -- Inputs mapped to the divider component
	signal  nReset       :  std_logic; -- Reset signal that resets every bits of 
									   -- DigitBits into zero									  
    signal  nCalculate   :  std_logic; -- Indicates that the calculation should 
									   -- start. Since this is an asynchronous 
									   -- external input, it needs to be 
									   -- synchronized for use			   
    signal  Divisor      :  std_logic; -- Indicates if we are inputting a dividend 
									   -- value or a divisor value  
    signal  KeypadRdy    :  std_logic; -- Indicates that a key is ready
    signal  Keypad       :  std_logic_vector(3 downto 0); -- Digit input from keypad (0~F)													  
    signal  CLK          :  std_logic := '0'; -- The source clock (1MHz)


    -- Outputs mapped to the divider component
    signal  HexDigit     :  std_logic_vector(3 downto 0); -- Hex digit to display 
														  -- (to segment decoder)												  
    signal  DecoderEn    :  std_logic; -- Enable for the digit decoder
    signal  DecoderBit   :  std_logic_vector(3 downto 0); -- Digit to display 
														  -- (to digit decoder)

    -- Quotient (the calculation result)
    signal  quotient  :  std_logic_vector(19 downto 0) := (others => '0');
	
    -- Indicates the end of simulation
    signal  END_SIM  :  BOOLEAN := FALSE;    

begin

    -- Mapping the Unit Under Test
    UUT : SerialDivider_16_20_bit
        generic map (NUM_NIBBLES => NUM_NIBBLES)
        port map  (
            nReset      => nReset,
            nCalculate  => nCalculate,
            Divisor     => Divisor,
            KeypadRdy   => KeypadRdy,
            Keypad      => Keypad,
            HexDigit    => HexDigit,
            DecoderEn   => DecoderEn,
            DecoderBit  => DecoderBit,
            CLK         => CLK
        );

    -- Simulate 1 MHz clock
    CLK <= not CLK after 500 ns;
	
	-- Process Name:
	--		Test
	-- Description:
	--		Simulate pressing the keypad and check if the calculation results are correct using assert statements.
	--      Random test cases are generated by using "uniform" function in math_real library that gives a random 
	--      real number in the range of [0,1]. The number is then scaled for use.
	--		Test cases handled in the simulation are:
	--			a. 10 general cases of random dividend and divisor
	--		    b. 10 edge cases of very large dividend and very small divisor pairs.
	--			c. Edge case of dividng by zero (FFFFF/00000).
	--			   It should output the error value (FFFFE for 20-bit mode, FFFE for 16-bit mode)
	--			d. Edge case of dividng zero (00000/FFFFF). 
	--			   It should output zero
    Test: process
		-- seed value 1 for random number generator
		variable seed1: positive := 25;
		-- seed value 2 for random number generator
		variable seed2: positive := 5000;     
		-- random real number [0,1] generated for dividend 
		variable rand_dividend_nonscale: real;  
		-- random real number [0,1] generated for divisor
		variable rand_divisor_nonscale: real;	 
		-- random dividend scaled and casted to an integer
		variable rand_dividend_scaled_integer: integer;
		-- random divisor scaled and casted to an integer		
		variable rand_divisor_scaled_integer: integer;
		-- random dividend in std_logic_vector
		variable rand_dividend_bitvector: std_logic_vector(19 downto 0);
		-- random divisor in std_logic_vector
		variable rand_divisor_bitvector: std_logic_vector(19 downto 0);
		-- random dividend's integer value in 16-bit mode(upper nibble lost)
		variable rand_dividend_integer_16bit: integer;
		-- random divisor's integer value in 16-bit mode(upper nibble lost)
		variable rand_divisor_integer_16bit: integer;
		-- Correct answer of the current division
		variable quotient_answer_integer: integer;
		-- Correct answer of the current division in 16-bit mode
		variable quotient_answer_integer_16bit: integer;
		-- The correct answer of current division in std_logic_vector
		variable quotient_answer_bitvector: std_logic_vector(19 downto 0);
		-- The correct answer of current division in 16-bit mode in std_logic_vector
		variable quotient_answer_bitvector_16bit: std_logic_vector(15 downto 0);
    
	begin
		-- Before beginning the test, initialize the following signals.
		nCalculate <= '1';
        KeypadRdy <= '0';
		Divisor <= '0';
		
		--------------------------------------------------------------------------
		-- ** General cases of random dividends and random divisors **
		-- Random dividend's range: [1, 1048575] for 20-bit mode
		--		                    [1, 65535] for 16-bit mode
		-- Random divisor's range: [1, 1048575] for 20-bit mode
		--						   [1, 65535] for 16-bit mode
		-- This loop tests 10 pairs of random dividend and divisor.
		--------------------------------------------------------------------------
	    for i in 0 to 10 loop
			-- Generate a random number for dividend
			uniform(seed1, seed2, rand_dividend_nonscale);   
			-- Rescale the random number's range from [0,1] to [1,1048575]
			rand_dividend_scaled_integer := integer(rand_dividend_nonscale * 1048574.0) + 1;
			-- Save the random dividend as std_logic_vector to extract nibbles from key inputs
			rand_dividend_bitvector 
					:= std_logic_vector(to_unsigned(rand_dividend_scaled_integer, rand_dividend_bitvector'length)); 
			-- Save the integer value of the random dividend in 16-bit mode. It loses the upper nibble
			rand_dividend_integer_16bit := to_integer(unsigned(rand_dividend_bitvector(15 downto 0)));
			
			-- Generate a random number for divisor
			uniform(seed1, seed2, rand_divisor_nonscale);
			-- Rescale the random number's range from [0,1] to [1,1048575]
			rand_divisor_scaled_integer := integer(rand_divisor_nonscale * 1048574.0) + 1;
			-- Save the random divisor as std_logic_vector to extract nibbles from key inputs
			rand_divisor_bitvector 
					  := std_logic_vector(to_unsigned(rand_divisor_scaled_integer, rand_divisor_bitvector'length));
			-- Save the integer value of the random divisor in 16-bit mode. It loses the upper nibble
			rand_divisor_integer_16bit := to_integer(unsigned(rand_divisor_bitvector(15 downto 0)));

			-- Calculate the correct answer of the division in 20-bit mode
			quotient_answer_integer := rand_dividend_scaled_integer / rand_divisor_scaled_integer;
			-- Calculate the correct answer of the division in 16-bit mode
			quotient_answer_integer_16bit := rand_dividend_integer_16bit / rand_divisor_integer_16bit;
			-- Save the correct answer of 20-bit mode division as std_logic_vector
			quotient_answer_bitvector 
					   := std_logic_vector(to_unsigned(quotient_answer_integer, quotient_answer_bitvector'length)); 
			-- Save the correct answer of 16-bit mode division as std_logic_vector
			quotient_answer_bitvector_16bit 
			 := std_logic_vector(to_unsigned(quotient_answer_integer_16bit, quotient_answer_bitvector_16bit'length)); 
			
			-- Toggle nReset to clear the DigitBits register between calculations.
			nReset <= '0';
			wait for 100 us;
			nReset <= '1';

			-- Input the dividend value first
			Divisor <= INPUT_DIVIDEND;
			
			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the first nibble of the dividend
			Keypad <= rand_dividend_bitvector(19 downto 16);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the second nibble of the dividend
			Keypad <= rand_dividend_bitvector(15 downto 12);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the third nibble of the dividend
			Keypad <= rand_dividend_bitvector(11 downto 8);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;
			
			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the fourth nibble of the dividend
			Keypad <= rand_dividend_bitvector(7 downto 4);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;
			
			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the fifth nibble of the dividend
			Keypad <= rand_dividend_bitvector(3 downto 0);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;
			
			-- Input the divisor value
			Divisor <= INPUT_DIVISOR;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the first nibble of the divisor
			Keypad <= rand_divisor_bitvector(19 downto 16);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the second nibble of the divisor
			Keypad <= rand_divisor_bitvector(15 downto 12);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the third nibble of the divisor
			Keypad <= rand_divisor_bitvector(11 downto 8);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
			-- Reset ready flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;
			
			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the fourth nibble of the divisor
			Keypad <= rand_divisor_bitvector(7 downto 4);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble.
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the fifth nibble of the divisor
			Keypad <= rand_divisor_bitvector(3 downto 0);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT); 
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Press calculate
			nCalculate <= '0';
			wait for 200 us;
			nCalculate <= '1';

			wait for 10 ms;

			-- Read the quotient (calculation result)
			-- Digit 14
			wait until std_match(DecoderBit, "1110");
			quotient(3 downto 0) <= HexDigit;

			-- Digit 13
			wait until std_match(DecoderBit, "1101");
			quotient(7 downto 4) <= HexDigit;

			-- Digit 12
			wait until std_match(DecoderBit, "1100");
			quotient(11 downto 8) <= HexDigit;

			-- Digit 11
			wait until std_match(DecoderBit, "1011");
			quotient(15 downto 12) <= HexDigit;
			
			-- Digit 10
			wait until std_match(DecoderBit, "1010");
			quotient(19 downto 16) <= HexDigit;

			wait for 10 us;

			-- Check if the division was done correctly.		
			-- If it is a 20-bit division
			if(NUM_NIBBLES = 5) then
				assert(std_match(quotient, quotient_answer_bitvector))
					report "Error: " & integer'image(rand_dividend_scaled_integer)
									 & " / "
									 & integer'image(rand_divisor_scaled_integer)
									 & " 's correct answer is "
									 & integer'image(quotient_answer_integer)
									 & ". The calculation's result was "
									 & integer'image(to_integer(unsigned(quotient)))
									 & " (numbers expressed in decimal)"
					severity ERROR;
					
			-- If it is a 16-bit division
			elsif(NUM_NIBBLES = 4) then
				assert(std_match(quotient, "0000" & quotient_answer_bitvector_16bit))
					report "Error: " & integer'image(rand_dividend_integer_16bit)
									 & " / "
									 & integer'image(rand_divisor_integer_16bit)
									 & " 's correct answer is "
									 & integer'image(quotient_answer_integer_16bit)
								     & ". The calculation's result was "
									 & integer'image(to_integer(unsigned(quotient)))
									 & " (numbers expressed in decimal)"
					severity ERROR;       
			end if;     
		end loop; 
		
		---------------------------------------------------------------------------
		-- ** Edge cases of random VERY LARGE dividends and VERY SMALL divisors **
		-- Randomly generates 10 pairs of very large dividend and very small divisor
		-- (which gives a very large quotient)
		-- The random dividend' range is [1032768, 1048575] for 20-bit mode
		--								 [49728, 65535] for 16-bit mode
		-- The random divisor's range is [1, 6] for both 20-bit and 16-bit modes
		---------------------------------------------------------------------------
        for i in 0 to 10 loop
			-- Generate a random number for dividend
			uniform(seed1, seed2, rand_dividend_nonscale);
			-- Rescale the random number's range to [1032768, 1048575] (very large)
			rand_dividend_scaled_integer := integer(rand_dividend_nonscale*15807.0) + 1032768;
			-- Save the random dividend as std_logic_vector to extract nibbles as key inputs
			rand_dividend_bitvector 
					:= std_logic_vector(to_unsigned(rand_dividend_scaled_integer, rand_dividend_bitvector'length)); 
			-- Save the integer value of the random dividend in 16-bit mode. It loses the upper nibble
			rand_dividend_integer_16bit := to_integer(unsigned(rand_dividend_bitvector(15 downto 0)));
		
			-- Generate a random number for divisor
			uniform(seed1, seed2, rand_divisor_nonscale);
			-- Rescale the random number's range to [1, 6] (very small)
			rand_divisor_scaled_integer := integer(rand_divisor_nonscale*5.0) + 1;
			-- Save the random divisor as std_logic_vector to extract nibbles as key inputs
			rand_divisor_bitvector 
					  := std_logic_vector(to_unsigned(rand_divisor_scaled_integer, rand_divisor_bitvector'length));
			-- Save the integer value of the random divisor in 16-bit mode. It loses the upper nibble
			rand_divisor_integer_16bit := to_integer(unsigned(rand_divisor_bitvector(15 downto 0)));

			-- Calculate the correct answer of the division in 20-bit mode
			quotient_answer_integer := rand_dividend_scaled_integer / rand_divisor_scaled_integer;
			-- Calculate the correct answer of the division in 16-bit mode
			quotient_answer_integer_16bit := rand_dividend_integer_16bit / rand_divisor_integer_16bit;
			-- Save the correct answer of 20-bit mode division as std_logic_vector
			quotient_answer_bitvector 
				       := std_logic_vector(to_unsigned(quotient_answer_integer, quotient_answer_bitvector'length)); 
			-- Save the correct answer of 16-bit mode division as std_logic_vector
			quotient_answer_bitvector_16bit 
			 := std_logic_vector(to_unsigned(quotient_answer_integer_16bit, quotient_answer_bitvector_16bit'length)); 
			
			-- Toggle nReset to clear the DigitBits register between calculations.
			nReset <= '0';
			wait for 100 us;
			nReset <= '1';

			-- Input the dividend value first
			Divisor <= INPUT_DIVIDEND;
			
			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the first nibble of the dividend
			Keypad <= rand_dividend_bitvector(19 downto 16);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the second nibble of the dividend
			Keypad <= rand_dividend_bitvector(15 downto 12);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the third nibble of the dividend
			Keypad <= rand_dividend_bitvector(11 downto 8);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;
			
			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the fourth nibble of the dividend
			Keypad <= rand_dividend_bitvector(7 downto 4);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;
			
			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the fifth nibble of the dividend
			Keypad <= rand_dividend_bitvector(3 downto 0);
			wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;
			
			-- Input the divisor value
			Divisor <= INPUT_DIVISOR;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the first nibble of the divisor
			Keypad <= rand_divisor_bitvector(19 downto 16);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT); 
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the second nibble of the divisor
			Keypad <= rand_divisor_bitvector(15 downto 12);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the third nibble of the divisor
			Keypad <= rand_divisor_bitvector(11 downto 8);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;
			
			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the fourth nibble of the divisor
			Keypad <= rand_divisor_bitvector(7 downto 4);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Set keypad ready flag as we are going to input a value using keypad
			KeypadRdy <= '1';
			wait for 5 us;
			-- Input the fifth nibble of the divisor
			Keypad <= rand_divisor_bitvector(3 downto 0);
			wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
			-- Reset the flag since we are done with inputting the current nibble
			KeypadRdy <= '0';
			wait for 20 us;

			-- Press the calculate button.
			nCalculate <= '0';
			wait for 200 us;
			nCalculate <= '1';

			wait for 10 ms;

			-- Read the quotient (calculation result)
			-- Digit 14
			wait until std_match(DecoderBit, "1110");
			quotient(3 downto 0) <= HexDigit;

			-- Digit 13
			wait until std_match(DecoderBit, "1101");
			quotient(7 downto 4) <= HexDigit;

			-- Digit 12
			wait until std_match(DecoderBit, "1100");
			quotient(11 downto 8) <= HexDigit;

			-- Digit 11
			wait until std_match(DecoderBit, "1011");
			quotient(15 downto 12) <= HexDigit;
			
			-- Digit 10
			wait until std_match(DecoderBit, "1010");
			quotient(19 downto 16) <= HexDigit;

			wait for 10 us;

			-- Check if the division was done correctly.
			-- If it is a 20-bit division
			if(NUM_NIBBLES = 5) then
				assert(std_match(quotient, quotient_answer_bitvector))
					report "Error: " & integer'image(rand_dividend_scaled_integer)
									 & " / "
									 & integer'image(rand_divisor_scaled_integer)
									 & " 's correct answer is "
									 & integer'image(quotient_answer_integer)
									 & ". The calculation's result was "
									 & integer'image(to_integer(unsigned(quotient)))
									 & " (numbers expressed in decimal)"
					severity ERROR;
					
			-- If it is a 16-bit division
			elsif(NUM_NIBBLES = 4) then
				assert(std_match(quotient, "0000" & quotient_answer_bitvector_16bit))
					report "Error: " & integer'image(rand_dividend_integer_16bit)
									 & " / "
									 & integer'image(rand_divisor_integer_16bit)
									 & " 's correct answer is "
									 & integer'image(quotient_answer_integer_16bit)
								     & ". The calculation's result was "
									 & integer'image(to_integer(unsigned(quotient)))
									 & " (numbers expressed in decimal)"
					severity ERROR;       
			end if;     
		end loop; 
		
		
		--------------------------------------------------------------------------------------
		-- ** Edge case of dividing by zero. **
		-- Calculate xFFFFF/x00000 (for 20-bit mode) or
		--           xFFFF/x0000 (for 16-bit mode)
		-- It should output the error value of xFFFFE(for 20-bit mode) or xFFFE(for 16-bit mode)
		--------------------------------------------------------------------------------------
		-- Toggle nReset to clear the DigitBits register between calculations.
		nReset <= '0';
		wait for 100 us;
		nReset <= '1';

		-- Input the dividend value first
		Divisor <= INPUT_DIVIDEND;
		
		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the first nibble of the dividend.
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the second nibble of the dividend.
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the third nibble of the dividend.
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;
		
		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the fourth nibble of the dividend.
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;
		
		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the fifth nibble of the dividend.
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;
		
		-- Input the divisor value
		Divisor <= INPUT_DIVISOR;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the first nibble of the divisor
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT); 
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the second nibble of the divisor
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the third nibble of the divisor
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;
		
		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the fourth nibble of the divisor
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the fifth nibble of the divisor
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Press the calculate button.
		nCalculate <= '0';
		wait for 200 us;
		nCalculate <= '1';

		wait for 10 ms;

		-- Read the quotient (calculation result)
		-- Digit 14
		wait until std_match(DecoderBit, "1110");
		quotient(3 downto 0) <= HexDigit;

		-- Digit 13
		wait until std_match(DecoderBit, "1101");
		quotient(7 downto 4) <= HexDigit;

		-- Digit 12
		wait until std_match(DecoderBit, "1100");
		quotient(11 downto 8) <= HexDigit;

		-- Digit 11
		wait until std_match(DecoderBit, "1011");
		quotient(15 downto 12) <= HexDigit;
		
		-- Digit 10
		wait until std_match(DecoderBit, "1010");
		quotient(19 downto 16) <= HexDigit;

		wait for 10 us;

        -- Press the calculate button
        nCalculate <= '0';
        wait for 200 us;
        nCalculate <= '1';

        wait for 10 ms;

        -- Read the result digits
        -- Digit 14
        wait until std_match(DecoderBit, "1110");
        quotient(3 downto 0) <= HexDigit;

        -- Digit 13
        wait until std_match(DecoderBit, "1101");
        quotient(7 downto 4) <= HexDigit;

        -- Digit 12
        wait until std_match(DecoderBit, "1100");
        quotient(11 downto 8) <= HexDigit;

        -- Digit 11
        wait until std_match(DecoderBit, "1011");
        quotient(15 downto 12) <= HexDigit;
        
        -- Digit 10
        wait until std_match(DecoderBit, "1010");
        quotient(19 downto 16) <= HexDigit;

        wait for 10 us;

		-- Check if the division was done correctly.
		-- If it is a 20-bit division
		if(NUM_NIBBLES = 5) then
			assert(std_match(quotient, x"FFFFE"))
				report "Error: When you divide by zero, you must get a error value xFFFFE. But you got" 
								 & integer'image(to_integer(unsigned(quotient)))
								 & " (quotient expressed in decimal)"
				severity ERROR;
				
		-- If it is a 16-bit division				
		elsif(NUM_NIBBLES = 4) then
			assert(std_match(quotient, x"0FFFE"))	
				report "Error: When you divide by zero, you must get a error value xFFFE. But you got" 
								 & integer'image(to_integer(unsigned(quotient)))
								 & " (quotient expressed in decimal)"
				severity ERROR;       
		end if;

         
		--------------------------------------------------------------------------------------
		-- ** Edge case of dividing zero. **
		-- Calculate x00000/xFFFFF (for 20-bit mode) or
		--			 x0000/xFFFF (for 16-bit mode)
		-- It should output zero as the quotient.
		--------------------------------------------------------------------------------------
		-- Toggle nReset to clear the DigitBits register between calculations.
		nReset <= '0';
		wait for 100 us;
		nReset <= '1';

		-- Input the dividend value first
		Divisor <= INPUT_DIVIDEND;
		
		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the first nibble of the dividend.
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the second nibble of the dividend.
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the third nibble of the dividend.
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;
		
		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the fourth nibble of the dividend.
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;
		
		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the fifth nibble of the dividend.
		Keypad <= "0000"; --'0'
		wait until std_match(DecoderBit, RESET_DIVIDEND_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;
		
		-- Input the divisor value
		Divisor <= INPUT_DIVISOR;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the first nibble of the divisor
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT); 
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the second nibble of the divisor
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the third nibble of the divisor
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;
		
		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the fourth nibble of the divisor
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Set keypad ready flag as we are going to input a value using keypad
		KeypadRdy <= '1';
		wait for 5 us;
		-- Input the fifth nibble of the divisor
		Keypad <= "1111"; --'F'
		wait until std_match(DecoderBit, RESET_DIVISOR_KEYPADRDY_DIGIT);
		-- Reset the flag since we are done with inputting the current nibble
		KeypadRdy <= '0';
		wait for 20 us;

		-- Press the calculate button.
		nCalculate <= '0';
		wait for 200 us;
		nCalculate <= '1';

		wait for 10 ms;

		-- Read the quotient (calculation result)
		-- Digit 14
		wait until std_match(DecoderBit, "1110");
		quotient(3 downto 0) <= HexDigit;

		-- Digit 13
		wait until std_match(DecoderBit, "1101");
		quotient(7 downto 4) <= HexDigit;

		-- Digit 12
		wait until std_match(DecoderBit, "1100");
		quotient(11 downto 8) <= HexDigit;

		-- Digit 11
		wait until std_match(DecoderBit, "1011");
		quotient(15 downto 12) <= HexDigit;
		
		-- Digit 10
		wait until std_match(DecoderBit, "1010");
		quotient(19 downto 16) <= HexDigit;

		wait for 10 us;

		-- Check if the division was done correctly.
		-- If it is a 20-bit division
		if(NUM_NIBBLES = 5) then
			assert(std_match(quotient, x"00000"))
				report "Error: x00000 / xFFFFF 's correct answer is 0. The calculation's result was"
								 & integer'image(to_integer(unsigned(quotient)))
								 & " (quotient expressed in decimal)"
				severity ERROR;
				
		-- If it is a 16-bit division
		elsif(NUM_NIBBLES = 4) then
			assert(std_match(quotient, x"00000"))
				report "Error: x0000 / xFFFF 's correct answer is 0. The calculation's result was"
								 & integer'image(to_integer(unsigned(quotient)))
								 & " (quotient expressed in decimal)"
				severity ERROR;       
		end if;   	

        END_SIM <= TRUE;	           -- The simulation is over.
        report "Simulation Finished."; -- Print the message on TCL to announce the end of simulation

        wait;                          

    end process Test;
    
end TB_ARCHITECTURE;

configuration TESTBENCH_FOR_SerialDivider_16_20_bit of SerialDivider_16_20_bit_tb is
    for TB_ARCHITECTURE
        for UUT : SerialDivider_16_20_bit
            use entity work.SerialDivider_16_20_bit(behavioral);
        end for;
    end for;
end TESTBENCH_FOR_SerialDivider_16_20_bit;