-- SerialDivider_16_20_bit.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
 
--Entity Name: 
-- 	SerialDivider_16_20_bit
--
--Description: 
-- 	This is a serial divider that calculates a division of two 16-bit values or 
--	20-bit values in serial. The actual hardware system has 15 LEDs in total with 
--  5 LEDS for Dividend, 5 LEDS for Divisor, and 5 LEDS for Quotient. It displays 
--  the digits on LED through multiplexing, turning on each digit approximately 
--  one milisecond out of every 10-20 miliseconds.
--  The system utilizes a 60-bit shift register named DigitBits for calculation,
--  receiving a new key input, and turning on one digit at a time.
--  Also, the system has assigned a label("CurDigit") to each digit of dividend, 
--  divisor, and quotient.
--  The CurDigit configuration of the LEDs on the hardware board is
--  	(Numbers indicate CurDigit)
-- 		 0  1  2  3  4  DIVIDEND
--  	 5  6  7  8  9  DIVISOR
--  	 10 11 12 13 14 QUOTIENT
--  Since the system can turn on only one digit(which is one nibble) at a time, 
--  the shift register DigitBits keeps shifting right while the system only 
--  displays the lowest nibble(digit) of Digitbits at a time on the current 
--  CurDigit's LED. 
--  Thus, to synchronize CurDigit to the system, CurDigit's value also keeps 
--  changing at a frequency of 1KHz. The 1KHz frequency is obtained from the 1MHz 
--  source clock by using the MuxCntr. DigitClkEn is enabled whenever MuxCntr 
--  reaches 11111111b(=1024). The CurDigit's changing sequence is
--  4, 3, 2, 1, 0, 15(Phantom digit), 9, 8, 7, 6, 5, 14, 13, 12, 11, 10.
--  Notice that 15 is a phantom digit introduced for the actual calculation.
--  The division calculation needs to be done while CurDigit is at 15, the phantom
--  digit. The non-restoring division algorithm used for this system is described 
--  below. 
--  Meanwhile, a new key input is received in the lowest nibble of DigitBits 
--  shift register shortly before(when MuxCntr = 3F6) the transition of 
--  CurDigit=4(DIVIDEND_RIGHTMOST_DIGIT) to CurDigit=3 or the transition of
--  CurDigit=9(DIVISOR_RIGHTMOST_DIGIT) to CurDigit=8. This is because the system 
--  always gets a new digit(nibble) from the right while the leftmost digit
--  (nibble) gets removed. In other words, the key inputs keep shifting left 
--  inside the range of bit=19 to bit=0.(Because each of dividend, divisor, quotient 
--  in this system are 5 digits, which sum up to 20 bits in total)
--  Note that for 16-bit division(when NUM_NIBBLES is set to 4), leftmost LEDs of 
--  dividend, divisor, and quotients always display zero since we don't use those 
--  digits.
--
--            *How Non-Restoring Division Algorithm works in this program*
--	While CurDigit=15(Phantom Digit), the division is executed.
--  In the calculation state, the data sequence inside DigitBits register is as 
--  follows.
--
--  				   --DigitBits(when CurDIgit = Phantom Digit(15))--
--  Bits     59	55			39	35 		        19 15       0
--  CurDigit 0  1  2  3  4  10  11  12  13  14  5  6  7  8  9
--           |--Dividend-|  |----Quotient----|  |---Divisor--| (*for 20-bit div)
--              |Dividend|      |--Quotient--|     |-Divisor-| (*for 16-bit div)

-- *Note that when we are calculating 16-bit divison(NUM_NIBBLES=4), we only use 
-- DigitBits(55 downto 40) as dividend, DigitBits(35 downto 20) as quotient, and 
-- DIgitBits(15 downto 0) as divisor. We don't use the top nibbles of each values
-- for division when we are in 16-bit division mode(NUM_NIBBLES=4).
-- Of course, on 20-bit division mode, all five nibbles are used for each values.
-- 
-- Now, the non-restoring division works as following:
-- *Summary: Repeat 16 sets of 17 bit serial add/subs (for 16-bit operation) or
--		     Repeat 20 sets of 21 bit serial add/subs (for 20-bit operation)
--
-- 1. Initialization: 
--    Reset the Remainder to 0.
--
-- 2. Repeat the following set of operations
--   2-1. Round 0: 
--     Substract the divisor's LSB from the dividend's MSB. Save the result in 
--     CalcResultBit so that we can push it into Remainder from right.
--	   Save the CarryFlag for next add/substractions. Shift the divisor to 
--     left by one bit because we need to add/substract each bits "in serial".
--   2-2. Round 1 ~ Round 19(for 20-bit div) / Round 1 ~ Round 15(for 16-bit div): 
--     Add/substract the LSB of Remainder with the LSB of Divisor. Save the 
--     CarryFlag for next add/substract, and push the CalcResultBit into the 
--	   Remainder from right. Shift both the Divisor and Remainder to left by one 
--     bit to repeat add/substracting LSBs for 19 rounds(for 20-bit division) or 
--	   for 15 rounds(for 16-bit division).
--   2-3. Round 20(for 20-bit div) / Round 16(for 16-bit div):
--     Calculate the final CarryFlag of current set since this final CarryFlag 
--	   determines whether the next set will be add or substract. Shift the dividend 
--	   to left by one bit for the next set of operations. Get the CalcResultBit 
--	   from the previous calculation into Remainder again.
--	   Bitwise add/sub operations for the current set is over at this point.
--   2-4. Round 21(for 20-bit division) / Round 17(for 16-bit division):
--	   Finally, push the final CarryFlag from the previous round into the Quotient. 
--	   Also, set the add/substract signal by using this CarryFlag as well. If the 
--	   final CarryFlag is '1', the next set of operations are substract. If it is 
--	   '0', the next set of operations are add.
--   In summary, 16 sets of 17 bit add/subs are executed for 16-bit division
--	 while 20 sets of 21 bit add/subs are executed for 20-bit division.
--
--	 *Divide by Zero (Error case): 
--			When dividng by zero, output "FFFFE"(when 20-bit division) or
--		    "FFFE"(when 16-bit division) as the error value
--
--Entity Generics/Ports Information
--	Generic
--		NUM_NIBBLES: Number of nibbles for dividend, divisor, and quotient
--                   If NUM_NIBBLES = 4, the system is in 16-bit division mode.
--                   If NUM_NIBBLES = 5, the system is in 20-bit division mode.
--	Inputs
--		nReset    : Reset signal that resets every bits of DigitBits into zero.
--		nCalculate: Indicates that the calculation should start. Since this is an 
--					asynchronous external input, it needs to be synchronized.
--	    Divisor   : Indicates if we are inputting a dividend value or a divisor value.
--      KeypadRdy : Indicates that a key is ready.
--      Keypad(3 downto 0): The input from keypad.(0~F)
--		CLK       : The source clock.(1MHz)
--                
--	Outputs
--		HexDigit  : Hex digit to display (to segment decoder)
--      DecoderEn : Enable for the digit decoder.
--		DecoderBit: Digit to display (to digit decoder)
--
--Revision History
--	01/10/2019	Sung Hoon Choi	Created
--	01/12/2019	Sung Hoon Choi  Fixed an error in the counters for division
--								Fixed an error in saving CarryFlag and shifting 
--								the dividend
--	01/13/2019	Sung Hoon Choi 	Added conditionals to HexDigits to mask leftmost 
--								LEDs when the system is in 16-bit division mode.
--								Changed some of the constant names to more 
--								reasonable ones
--	01/14/2019	Sung Hoon Choi 	Added process headers
--	01/15/2019	Sung Hoon Choi  Added comments to constants and signals.


entity  SerialDivider_16_20_bit  is
    generic(
        NUM_NIBBLES : integer := 5);
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

end  SerialDivider_16_20_bit;


architecture  behavioral  of  SerialDivider_16_20_bit  is

    -- General constants
    constant BITS_IN_NIBBLE: integer := 4; -- Number of bits in a nibble
	
	-- Board information
    constant NUM_DIGITROWS: integer := 3;  -- Number of digit rows on our board.
										   -- We have 3 rows of LED digits on the board 
										   -- (One row for dividend, one row for divisor, one row for quotient)
										   
    constant NUM_LEDS_IN_ROW: integer := 5;-- Our board has 5 LEDs per row. Thus, we have 15 LEDs in total

	-- Size of DigitBits shift register
	-- Since our board has 15 LEDs, Digitbits shift register needs 60 bits.
    constant DIGITBITS_MSB: integer := 59;
    constant DIGITBITS_LSB: integer := 0;  
    
    -- Size of Remainder register
	-- For 16-bit, Remainder: [15 downto 0]
	-- For 20-bit, Remainder: [19 downto 0]
    constant REMAINDER_MSB: integer := BITS_IN_NIBBLE * NUM_NIBBLES - 1;
	constant REMAINDER_LSB: integer := 0;
	
	--Sizes of addsub_bit_counter and addsub_set_counter
    constant ADDSUB_BIT_COUNTER_MAX : integer := BITS_IN_NIBBLE * NUM_NIBBLES + 1; 
															-- For 16-bit, addsub_bit_counter's range is 0 to 17
															-- For 20-bit, addsub_bit_counter's range is 0 to 21
    constant ADDSUB_SET_COUNTER_MAX : integer := BITS_IN_NIBBLE * NUM_NIBBLES;
															-- For 16-bit, addsub_set_counter's range is 0 to 16
															-- For 20-bit, addsub_set_counter's range is 0 to 20
    constant ADDSUB_COUNTER_MIN : integer := 0; -- Both counters start from 0
							
	
	-- Dividend, Divisor, Quotient's digit configuration information
	-- CurDigit allocations for dividend: 0  1  2  3  4
	-- CurDigit allocations for divisor : 5  6  7  8  9
	-- CurDigit allocations for quotient: 10 11 12 13 14
    constant DIVIDEND_RIGHTMOST_DIGIT : std_logic_vector(3 downto 0) 
		:= std_logic_vector(to_unsigned(NUM_LEDS_IN_ROW * 1 - 1, BITS_IN_NIBBLE)); -- DIVIDEND_RIGHTMOST_DIGIT: 4
	constant DIVIDEND_LEFTMOST_DIGIT: std_logic_vector(3 downto 0) 
		:= std_logic_vector(to_unsigned(0, BITS_IN_NIBBLE)); 					   -- DIVIDEND_LEFTMOST_DIGIT: 0                  
    constant DIVISOR_RIGHTMOST_DIGIT : std_logic_vector(3 downto 0) 
		:= std_logic_vector(to_unsigned(NUM_LEDS_IN_ROW * 2 - 1, BITS_IN_NIBBLE)); -- DIVISOR_RIGHTMOST_DIGIT: 9
	constant DIVISOR_LEFTMOST_DIGIT: std_logic_vector(3 downto 0) 
		:= std_logic_vector(to_unsigned(NUM_LEDS_IN_ROW, BITS_IN_NIBBLE)); 		   -- DIVISOR_LEFTMOST_DIGIT: 5    
    constant QUOTIENT_RIGHTMOST_DIGIT: std_logic_vector(3 downto 0)               
		:= std_logic_vector(to_unsigned(NUM_LEDS_IN_ROW * 3 -1, BITS_IN_NIBBLE));  -- QUOTIENT_RIGHTMOST_DIGIT: 14
    constant QUOTIENT_LEFTMOST_DIGIT: std_logic_vector(3 downto 0) 
		:= std_logic_vector(to_unsigned(NUM_LEDS_IN_ROW * 2, BITS_IN_NIBBLE));     -- QUOTIENT_LEFTMOST_DIGIT: 10
    
    -- Phantom digit used for calculation
    -- The division is done while CurDigit is equal to CALC_PHANTOM_DIGIT
    constant CALC_PHANTOM_DIGIT: std_logic_vector(3 downto 0) 
		:= std_logic_vector(to_unsigned(NUM_LEDS_IN_ROW * 3, BITS_IN_NIBBLE)); -- CALC_PHANTOM_DIGIT: 15

	-- Location of Divisor in DigitBits register during calculation
	-- For 16-bit, Divisor is 15 downto 0
	-- For 20-bit, Divisor is 19 downto 0
    constant DIVISOR_CALC_MSB: integer := BITS_IN_NIBBLE * NUM_NIBBLES - 1;	-- For 16-bit, DIVISOR_CALC_MSB = 15
																			-- For 20-bit, DIVISOR_CALC_MSB = 19
    constant DIVISOR_CALC_LSB: integer := BITS_IN_NIBBLE * NUM_LEDS_IN_ROW * 0; -- For 16-bit, DIVISOR_CALC_LSB = 0
																			    -- For 20-bit, DIVISOR_CALC_LSB = 0
    
    --Location of Quotient in DigitBits register during calculation
	--For 16-bit, Quotient is 35 downto 20
	--For 20-bit, Quotient is 39 downto 20
    constant QUOTIENT_CALC_MSB: integer := BITS_IN_NIBBLE * NUM_LEDS_IN_ROW + BITS_IN_NIBBLE * NUM_NIBBLES - 1;
																			-- For 16-bit, QUOTIENT_CALC_MSB = 35
																			-- For 20-bit, QUOTIENT_CALC_MSB = 39
    constant QUOTIENT_CALC_LSB: integer := BITS_IN_NIBBLE * NUM_LEDS_IN_ROW * 1;
																			-- For 16-bit, QUOTIENT_CALC_LSB = 20
																			-- For 20-bit, QUOTIENT_CALC_LSB = 20
	 
	 --Location of Dividend in DigitBits register during calculation
	 --For 16-bit, Dividend is 55 downto 40
	 --For 20-bit, Dividend is 59 downto 40
    constant DIVIDEND_CALC_MSB: integer := BITS_IN_NIBBLE * NUM_LEDS_IN_ROW * 2 +  BITS_IN_NIBBLE * NUM_NIBBLES -1;
																			-- For 16-bit, DIVIDEND_CALC_MSB = 55
																			-- For 20-bit, DIVIDEND_CALC_MSB = 59
    constant DIVIDEND_CALC_LSB: integer := BITS_IN_NIBBLE * NUM_LEDS_IN_ROW * 2;
																			-- For 16-bit, DIVIDEND_CALC_LSB = 40
																			-- For 20-bit, DIVIDEND_CALC_LSB = 40
    
    -- MSB of KeyIn data slot in DigitBits register
    constant KEYIN_MSB: integer := 19; -- New key inputs only move inside [19 downto 0] region of DigitBits register
									   -- Nibble of [19 downto 16] is not used for calculation and/or division when 
								       -- the system is in 16-bit division mode

    -- Values that indicate whether we are inputting a divisor or a dividend
    constant INPUT_DIVISOR: std_logic := '1';  -- divisor
    constant INPUT_DIVIDEND: std_logic := '0'; -- dividend

	-- Values that indicate whether the arithemtic operation is add or substract
	constant SUBSTRACT: std_logic := '1'; -- substract
	constant ADD: std_logic := '0';       -- add

    -- Keypad signals
    signal  HaveKey: std_logic; -- Have a key from the keypad
    signal  KeypadRdyS: std_logic_vector(2 downto 0); -- synchronized KeypadRdy

	-- Synchronized Calculate button input
    signal  nCalculate_Sync : std_logic_vector(2 downto 0); -- Since nCalculate is an asynchronous external signal,
															-- it needs to be synchronized

    -- LED multiplexing signals
    signal  MuxCntr     :  unsigned(9 downto 0); -- multiplex counter
									             -- (to divide 1 MHz to 1 KHz)													 
    signal  DigitClkEn  :  std_logic;	 -- enable for the digit clock
    signal  CalcInEn    :  std_logic; 	 -- near end of a muxed digit 
										 -- (to enable calculations)												  
    signal  CurDigit    :  std_logic_vector(3 downto 0); -- current mux digit

    --  signals to select shift register operation
    --     ShiftOp = 0  ==>  hold
    --     ShiftOp = 1  ==>  calculate shift
    --     ShiftOp = 2  ==>  keypad input shift
    --     ShiftOp = 3  ==>  display shift
    signal    ShiftOp       :  std_logic_vector(1 downto 0);
    constant  ShiftOpHOLD   :  std_logic_vector(1 downto 0) := "00";
    constant  ShiftOpCALC   :  std_logic_vector(1 downto 0) := "01";
    constant  ShiftOpKEYIN  :  std_logic_vector(1 downto 0) := "10";
    constant  ShiftOpSHIFT  :  std_logic_vector(1 downto 0) := "11";

	
    --  15 stored hex digits (60 bits) in a shift register
    signal  DigitBits  :  std_logic_vector(DIGITBITS_MSB downto DIGITBITS_LSB) := (others => '0');

    --  Signals for division arithmetics.
	signal  Add_or_Sub     : std_logic := SUBSTRACT; -- 0 for add, 1 for subtract
    signal  CalcResultBit  :  std_logic;  -- sum/difference output bit	
    signal  CarryFlag      :  std_logic;  -- stored carry flag
    signal  Remainder  :  std_logic_vector(REMAINDER_MSB downto REMAINDER_LSB);	-- Remainder of division
																				-- Also used as a temporary output
																				-- for add/sub loops
    signal  Calc_Rdy : std_logic := '0'; 	   -- Ready to run calculation. Set by pressing Calculate button.											   
    signal  Start_Calc   : std_logic := '0';   -- Flag that signals the start of calculation										   
    signal  Calc_Finished  : std_logic := '0'; -- Flag that signals the end of calculation

	-- counters for division operation.
    signal addsub_bit_counter: integer range ADDSUB_COUNTER_MIN to ADDSUB_BIT_COUNTER_MAX := 0;
																			 -- Counter used for counting the
																		     -- number of add/substract operations
																			 -- (inner loop)
    signal addsub_set_counter: integer range ADDSUB_COUNTER_MIN to ADDSUB_SET_COUNTER_MAX := 0;
																		     -- Counter used for counting the 
												                             -- number of sets of add/substract
																			 -- operations (outer loop)


begin

    -- Process Name: 
	--		KeyDetection
	-- Description:
	--		Detect edge (and key) on KeypadRdy and set/reset HaveKey.
    KeyDetection: process(CLK)
    begin
        if rising_edge(CLK) then
		
            -- shift the keypad ready signal to synchronize and edge detect
            KeypadRdyS  <=  KeypadRdyS(1 downto 0) & KeypadRdy;
			
            -- have a key if have one already that hasn't been processed or a
            -- new one is coming in (rising edge of KeypadRdy), reset if on
            -- the last clock of Digit 4 or Digit 9 (depending on position of
            -- Divisor switch) and held otherwise
            if  (std_match(KeypadRdyS, "01-")) then
                -- set HaveKey on rising edge of synchronized KeypadRdy
                HaveKey <=  '1';
            elsif ((DigitClkEn = '1') and (CurDigit = DIVIDEND_RIGHTMOST_DIGIT) and 
				  (Divisor = INPUT_DIVIDEND)) then
                -- reset HaveKey if on Dividend and current digit is 4
                HaveKey <=  '0';
            elsif ((DigitClkEn = '1') and (CurDigit = DIVISOR_RIGHTMOST_DIGIT) and 
				  (Divisor = INPUT_DIVISOR)) then
                -- reset HaveKey if on Divisor and current digit is 9
                HaveKey <=  '0';
            else
                -- otherwise hold the value
                HaveKey <=  HaveKey;
            end if;         
        end if;
    end process KeyDetection;



    -- Process Name:
	--		MuxCounter
	-- Description:
	-- 		Counter for mux rate of 1 KHz (1 MHz / 1024)
    MuxCounter: process(CLK)
    begin
        -- count on the rising edge (clear on reset)
        if rising_edge(CLK) then
            if (nReset = '0') then
                MuxCntr <= (others => '0');
            else
                MuxCntr <= MuxCntr + 1;
            end if;
        end if;

    end process MuxCounter;


    -- the multiplex counter is also used for controlling the operation of
    -- the circuit - DigitClkEn signals the end of a multiplexed digit
    -- (MuxCntr = 3FF) and CalcInEn signals time periods in which calculations
    -- or inputting may be done (16 clocks - MuxCntr = 11111xxxx0)

    DigitClkEn  <=  '1'  when (MuxCntr = "1111111111")  else
                    '0';
    CalcInEn    <=  '1'  when (std_match(MuxCntr, "11111----0"))  else
                    '0';



	-- Process Name: 
	--		ChangeCurDigit
	-- Description:
    -- 		Create the counter to output the current digit
	-- 		The order is 4, 3, 2, 1, 0, 15(Phantom digit), 9, 8, 7, 6, 5, 14, 13, 12, 11, then 10
    -- 		When reset, the current digit is set to 4
    -- 		The current digit is incremented when DigitClkEn is set (when MuxCntr reaches 1024)
    -- 		When current digit is 15, the system does not display a digit. Instead, it does the calculation
    ChangeCurDigit: process (CLK)
    begin
        if(rising_edge(clk)) then
            if(nReset = '0') then
                CurDigit <= "0100";	--When reset, CurDigit goes back to 4.
            elsif (DigitClkEn = '1') then
                CurDigit(0) <= not curDigit(0);
                CurDigit(1) <= CurDigit(1) xor not CurDigit(0);
                CurDigit(2) <= (CurDigit(2) and (CurDigit(1) or CurDigit(0))) or 
							    not(CurDigit(2) or CurDigit(1) or CurDigit(0));
                if(std_match(CurDigit,"-000")) then
                    CurDigit(3) <= not CurDigit(3);
                end if;
                if(std_match(CurDigit, CALC_PHANTOM_DIGIT)) then
                    CurDigit <= DIVISOR_RIGHTMOST_DIGIT; 
                elsif(std_match(CurDigit, DIVISOR_LEFTMOST_DIGIT)) then
                    CurDigit <= QUOTIENT_RIGHTMOST_DIGIT;
                elsif(std_match(CurDigit, QUOTIENT_LEFTMOST_DIGIT)) then
                    CurDigit <= DIVIDEND_RIGHTMOST_DIGIT;
                end if;
             else
                CurDigit <= CurDigit;
             end if;
        end if;     
    end process ChangeCurDigit;


    -- Always enable the digit decoder
    DecoderEn  <=  '1';

    -- Output the current digit to the digit decoder
    DecoderBit  <=  CurDigit;

    -- The hex digit to output is just the low nibble of the shift register
	-- When the system is in 16-bit mode (NUM_NIBBLES = 4), the leftmost digits of dividend, divisor, and quotient
	-- are cleared to zero
    HexDigit  <=  "0000" when ((NUM_NIBBLES = 4) and 
                               ((CurDigit = DIVIDEND_LEFTMOST_DIGIT) or 
                               (CurDigit = DIVISOR_LEFTMOST_DIGIT) or 
                               (CurDigit = QUOTIENT_LEFTMOST_DIGIT))) else 
                  DigitBits(3 downto 0);


    -- Shift register commands
    --    Set bit 0 if shifting for display or doing a calculation
    --    Set bit 1 if shifting for display or inputting a key (MuxCntr = 3F6)	
				-- If CurDigit is not the phantom digit, keep shifting the bits for display
    ShiftOp  <=  ShiftOpSHIFT  when ((DigitClkEn = '1') and 
                                     not (CurDigit = CALC_PHANTOM_DIGIT))  else
				-- Shift in the new key input if you have the key and CurDigit is 4(for dividend) or 9(for divisor)
                 ShiftOpKEYIN  when ((CalcInEn = '1') and
                                     (Start_Calc = '0') and
                                     (HaveKey = '1') and
                                     (std_match(MuxCntr, "-----1011-") and
                                      (((CurDigit = DIVIDEND_RIGHTMOST_DIGIT) and (Divisor = INPUT_DIVIDEND)) or
                                      ((CurDigit = DIVISOR_RIGHTMOST_DIGIT) and (Divisor = INPUT_DIVISOR))))) else
				-- Start calculation when the start flag is set and CurDigit is the phantom digit
                 ShiftOpCALC   when ((Start_Calc = '1') and
                                     (CurDigit = CALC_PHANTOM_DIGIT))  else
				-- Otherwise just hold the bits
                 ShiftOpHOLD;

				 

    -- Process Name:
    --     Sync_nCalculate
    -- Description:
    --     Since nCalculate is an asynchronous external input, synchronize and 
	--     edge-detect the nCalculate signal.
    Sync_nCalculate: process(CLK)
    begin
        if rising_edge(CLK) then
			-- Synchronize nCalculate/Edge-detect nCalculate
            nCalculate_Sync <= nCalculate_Sync(1 downto 0) & nCalculate;
        end if;
    end process Sync_nCalculate;



	-- Process Name: 
	--		ShiftOperations
	-- Description:
    --    ShiftHold:    DigitBits(59..0) = DigitBits(59..0)
    --    ShiftCalc:    Repeat 16 sets of 17 bit serial add/sub (for 16-bit division)
	--					Repeat 20 sets of 21 bit serial bit serial add/sub (for 20-bit division)
    --    ShiftKeyIn:   DigitBits(47..0) = DigitBits(47..16) |
    --                                     DigitBits(11..0) | Keypad(3..0)
    --    ShiftDisplay: DigitBits(47..0) = DigitBits(3..0) | DigitBits(47..4)
	--	  
	--	  *****Inside ShiftCalc (Non-restoring division algorithm)******
	--	  When calculating,the bit arrangements inside DigitBits are as follows.
	--  				   --DigitBits(when CurDIgit = Phantom Digit(15))--
	--  	Bits     59	55			39	35 		        19 15       0
	--  	CurDigit 0  1  2  3  4  10  11  12  13  14  5  6  7  8  9
	--      	     |--Dividend-|  |----Quotient----|  |---Divisor--| (for 20-bit div)
	--          	    |Dividend|      |--Quotient--|     |-Divisor-| (for 16-bit div)
    --    	
	--		Divisor: 19-0 (for 20-bit operation)
	--				 15-0 (for 16-bit operation)
	--		Quotient: 39-20(for 20-bit operation)
	--				  35-20(for 16-bit operation)
    --      Dividend: 59-40(for 20-bit operation)
	--				  55-40(for 16-bit operation)
	--
	-- 	  1. Initialization: 
	--    	Reset the Remainder to 0.
	--
	-- 	  2. Repeat the following set of operations
	--      (Repeat 20 sets of 21-bit add/sub for 20-bit division, 
	--		     or 16 sets of 17-bit add/sub for 16-bit division) 
	--      2-1. Round 0: 
	--     		Substract the divisor's LSB from the dividend's MSB. Save the 
	--			result in CalcResultBit so that we can push it into Remainder from 
	--			right. Save the CarryFlag for next add/substracts.
	--			Shift the divisor to left by one bit because we need to 
	--			add/substract each bits "in serial".
	--   	2-2. Round 1 ~ Round 19(for 20-bit) / Round 1 ~ Round 15(for 16-bit): 
	--     		Add/substract the LSB of Remainder with the LSB of Divisor. 
	--			Save the CarryFlag for next add/substract, and push the 
	--			CalcResultBit into the Remainder from right. Shift both the Divisor 
	--			and Remainder to left by one bit to repeat add/substracting LSBs 
	--          for 19 rounds(for 20-bit) or for 15 rounds(for 16-bit).
	--      2-3. Round 20(for 20-bit division) / Round 16(for 16-bit division):
	--          Calculate the final CarryFlag of current set since this final 
	--			CarryFlag determines whether the next set will be add or substract. 
	--			Shift the dividend to left by one bit for the next set of 
	--			operations. Get the CalcResultBit from the previous calculation 
	--    		into Remainder again. Bitwise add/sub operations for current set is 
	--			over at this point.
	--      2-4. Round 21(for 20-bit division) / Round 17(for 16-bit division):
	--	       Finally, push the final CarryFlag from the previous round into the 
	--		   Quotient. Also, set the add/substract signal by using this CarryFlag 
	--		   as well. If the final CarryFlag is '1', next set of operations is 
	--		   substract. If it is '0', next set of operations is add.
	--		
	--		Divide by Zero case (Error case): 
	--			When dividng by zero, output "FFFFE"(when 20-bit division) or
	--		    "FFFE"(when 16-bit division)
    ShiftOperations: process(CLK)
    begin
        -- Shift on the rising edge
        if rising_edge(CLK) then
			-- If the edge of nCalculate is detected
            if (std_match(nCalculate_Sync, "10-")) then
		        -- Initialize Remainder to zero.
                Remainder <= (others => '0');    
                -- Since nCalculate key is detected, we are ready to calculate
                Calc_Rdy <= '1';
				-- Clear Calc_Finished flag since we are about to start the calculation
                Calc_Finished <= '0';
                -- No borrow at the beginning (We begin with substract)
                CarryFlag <= '1';
                -- We start with substraction since Remainder's initial MSB is '0'
                Add_or_Sub <= SUBSTRACT;
            end if;

            -- Update Start_Calc flag only when DigitClkEn is set to prevent executing the calculation when 
			-- a new key input is received in the middle of calculating state
            if (DigitClkEn = '1') then
                Start_Calc <= Calc_Rdy;
            end if;

			-- Operations for ShiftOpHOLD, ShiftOpCALC, ShiftOpKEYIN, ShiftOpSHIFT
            case  ShiftOp  is
				-- Hold the bits inside DigitBits register
                when ShiftOpHOLD =>
                    DigitBits <= DigitBits;
				-- Do the actual division calculation
                when ShiftOpCALC =>
                    -- If we are dividing by zero (Divisor = 0)
                    if (to_integer(unsigned(DigitBits(DIVISOR_CALC_MSB downto DIVISOR_CALC_LSB))) = 0) then
                        -- Report error by outputting FFFE(for 16-bit) or FFFFE(for 20-bit)
                        DigitBits(QUOTIENT_CALC_MSB downto QUOTIENT_CALC_LSB+1) <= (others => '1');
                        DigitBits(QUOTIENT_CALC_LSB) <= '0';
                        -- Since we are already done with calculation, reset the Calc_Rdy and Start_Calc flags
                        Calc_Rdy <= '0';
  					    Start_Calc <= '0';
                    -- If the divisor is not zero, then do the calculation
                    else
                        -- If the calculation is not finished yet
                        if (Calc_Finished = '0') then
                            -- If we have repeated required number of sets of bitwise add/subs, set the flag
							-- For 16-bit division: 16 sets of 17 bit add/subs
							-- For 20-bit division: 20 sets of 21 bit add/subs
                            if(addsub_set_counter = ADDSUB_SET_COUNTER_MAX) then
                                Calc_Finished <= '1';
                            
							-- Round 0 (inner loop)
                            elsif (addsub_bit_counter = 0) then
                                -- Add/Substract divisor's LSB to dividend's MSB
								-- Save the result bit and carry.
                                CalcResultBit  <= DigitBits(DIVISOR_CALC_LSB) xor Add_or_Sub xor 
																	DigitBits(DIVIDEND_CALC_MSB) xor CarryFlag;
                                CarryFlag  <= (DigitBits(DIVIDEND_CALC_MSB) and CarryFlag) or
                                                 ((DigitBits(DIVISOR_CALC_LSB) xor Add_or_Sub) and 
												 DigitBits(DIVIDEND_CALC_MSB)) or ((DigitBits(DIVISOR_CALC_LSB) xor 
												 Add_or_Sub) and CarryFlag);
                                -- Shift left the divisor by one bit to keep add/substracting
                                DigitBits(DIVISOR_CALC_MSB downto DIVISOR_CALC_LSB) <= DigitBits(DIVISOR_CALC_LSB) & 
															 DigitBits(DIVISOR_CALC_MSB downto DIVISOR_CALC_LSB + 1); 
                                -- Increment the counter to go to next round
                                addsub_bit_counter <= addsub_bit_counter + 1;
                                
                            -- Round 1 ~ Round 15 (inner loop) for 16-bit division
							-- Round 1 ~ Round 19 (inner loop) for 20-bit division
                            elsif (addsub_bit_counter <= ADDSUB_BIT_COUNTER_MAX - 2) then
                                -- shift divisor for next add/sub 
                                DigitBits(DIVISOR_CALC_MSB downto DIVISOR_CALC_LSB) <= DigitBits(DIVISOR_CALC_LSB) & 
															 DigitBits(DIVISOR_CALC_MSB downto DIVISOR_CALC_LSB + 1);  
                                -- Add/Substract divisor's LSB with remainder's LSB
								-- Save the result bit and carry.
                                CalcResultBit <= DigitBits(DIVISOR_CALC_LSB) xor Add_or_Sub xor 
																		Remainder(REMAINDER_LSB) xor CarryFlag;
                                CarryFlag  <= (Remainder(REMAINDER_LSB) and CarryFlag) or
													  ((DigitBits(DIVISOR_CALC_LSB) xor Add_or_Sub) and 
													  Remainder(REMAINDER_LSB)) or
													  ((DigitBits(DIVISOR_CALC_LSB) xor Add_or_Sub) and CarryFlag);
                                -- Push the result bit from previous add/sub into the remainder
                                Remainder <= CalcResultBit & Remainder(REMAINDER_MSB downto REMAINDER_LSB + 1);
                                -- Increment the counter to go to next round
                                addsub_bit_counter <= addsub_bit_counter + 1;
                                
                            -- Round 16 (inner loop) for 16-bit division
							-- Round 20 (inner loop) for 20-bit division
                            elsif (addsub_bit_counter = ADDSUB_BIT_COUNTER_MAX - 1) then
                                -- Obtain the final carry bit
                                CarryFlag  <= (Remainder(REMAINDER_LSB) and CarryFlag) or
                                                 (Add_or_Sub and Remainder(0)) or
                                                 (Add_or_Sub and CarryFlag);
                                -- Shift(Rotate) dividend to left by one bit for the next set of arithmetic operations
                                DigitBits(DIVIDEND_CALC_MSB downto DIVIDEND_CALC_LSB) <= 
														  DigitBits(DIVIDEND_CALC_MSB-1 downto DIVIDEND_CALC_LSB) & 
														  DigitBits(DIVIDEND_CALC_MSB);
                                -- Push the result bit from previous add/sub into the remainder
                                Remainder <= CalcResultBit & Remainder(REMAINDER_MSB downto REMAINDER_LSB + 1);
                                -- Increment the counter to go to next round
                                addsub_bit_counter <= addsub_bit_counter + 1;
                                -- At this point, we are done with add/substracting bits for the current
								-- set of operations
								
                            -- Round 17 (inner loop) for 16-bit division
							-- Round 21 (inner loop) for 20-bit division
                            elsif (addsub_bit_counter = ADDSUB_BIT_COUNTER_MAX) then
                                -- Save the quotient bit obtained from the current set of add/sub operations
                                DigitBits(QUOTIENT_CALC_MSB downto QUOTIENT_CALC_LSB) <= 
											    DigitBits(QUOTIENT_CALC_MSB-1 downto QUOTIENT_CALC_LSB) & CarryFlag;
                                 -- If the final carry is '1', next set of operations are substractions
								 -- If the final carry is '0', next set of operations are adds
                                Add_or_Sub <= CarryFlag;
                                -- Reset the add/sub counter since we are done with the current set of operations 
								-- and about to start a new set of add/sub operations
                                addsub_bit_counter <= 0;
                                -- Increment the set counter (outer loop) to begin the next set of add/sub operations
                                addsub_set_counter <= addsub_set_counter + 1;       
                            end if;
							
						-- If the entire calculation is finished	
                        else
							-- There's no need to change DigitBits since we are done with calculation
                            DigitBits <= DigitBits;
                            -- Since we are done with calculation, reset both (addsub_bit_counter) and 
							-- (addsub_set_counter)
                            addsub_bit_counter <= 0;
							addsub_set_counter <= 0;
							-- Since we are done with calculation, reset Calc_Rdy and Start_Calc flags.
                            Calc_Rdy <= '0';
  					        Start_Calc <= '0';
                        end if;
                    end if;
				
				-- Save the new key input in the lowest nibble and accordingly shift left
				-- the bits in the keyinput area of DigitBits register.
                when ShiftOpKEYIN =>
                    DigitBits <= DigitBits(DIGITBITS_MSB downto KEYIN_MSB + 1) &
                                 DigitBits(KEYIN_MSB - BITS_IN_NIBBLE downto DIGITBITS_LSB) & Keypad;
				-- Rotate right the digits inside DIgitBits register to display
				-- all digits in the register (of course, one digit at a time)
                when ShiftOpSHIFT =>
                    DigitBits <= DigitBits(BITS_IN_NIBBLE -1 downto DIGITBITS_LSB) & 
												DigitBits(DIGITBITS_MSB downto BITS_IN_NIBBLE);
				-- Otherwise, just hold the data in DigitBits register
                when others =>
                    DigitBits <= DigitBits;
            end case;

            -- Use nReset to clear every bits in DigitBits register
            if (nReset = '0') then
                DigitBits <= (others => '0');
            end if;
        end if;
    end process ShiftOperations;

end  behavioral;