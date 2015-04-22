%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This module contains functions for bit manipulations.
%% @end


-module(bit_operations).

%% ====================================================================
%% Type definitions
%% ====================================================================
-type data()			::	0..255.	%% The end bitfield_value is not really 255, but what is supported by the OS and CPU.
-type bitfield_value()	::	0..255.	%% The end bitfield_value is not really 255, but what is supported by the OS and CPU.
-type bitfield_mask()	::	0..255.	%% The end bitfield_value is not really 255, but what is supported by the OS and CPU.
-type bit()				::	0 | 1.

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 bit_test/2,
		 bit_list_get/1,
		 bit_set/2, bit_set/3, bit_set/4,
		 bit_clear/2,
		 bit_get/2, bit_get/3]).

%% ====================================================================
%% @doc
%% Test a bit bitfield_value on a specified position inside the byte.
%% @end
-spec bit_test(data(), bit()) -> 0 | 1.
%% ====================================================================
bit_test(Byte, Bit) when Bit == 0 ->
	Byte band 1;
bit_test(Byte, Bit) when Bit > 0 ->
	%% B0 - Val band 1
	%% B1 - (Val band 2) bsr 1
	%% B2 - (Val band 4) bsr 2
	%% B3 - (Val band 8) bsr 3
	%% ...	
	BAND_MASK = erlang:round(math:pow(2,Bit)),
	(Byte band BAND_MASK) bsr Bit.

%% ====================================================================
%% @doc
%% Give the list of bits of the given byte.
%% @end
-spec bit_list_get(data()) -> list(bit()).
%% ====================================================================
bit_list_get(Byte) ->
	%% Find the number of bits of the given byte.
	Base = 255,
	BaseBitLength = 8,
	N = bit_list_get_loop(Byte, 1, Base),

	%% Compute the bit list.
	[begin
		 bit_test(Byte,Bit)
	 end || Bit <- lists:reverse((lists:seq(0, ((N*BaseBitLength)-1))))].

bit_list_get_loop(Byte, N, Base) when (Byte > (N*Base)) ->
	bit_list_get_loop(Byte, N+1, Base);
bit_list_get_loop(_Byte, N, _Base) ->
	N.
	
%% ====================================================================
%% @doc
%% Set the bit on the specified position in the byte.
%% TODO: It would be good to compare the length of data with bit,
%%       and reject the operation if bit > than bit length of data. 
%% @end
-spec bit_set(data(), bit()) -> data().
%% ====================================================================
bit_set(Byte, Bit) ->
	MASK = 1 bsl Bit,
	Byte bor MASK.

%% ====================================================================
%% @doc
%% Set or clear number of bits in a byte. The number of bits are specified
%% by the Value, the bit positions specified by the Mask. The bit=1 in the Mask
%% means what bit must set/clear. 
%% @end
-spec bit_set(data(), bitfield_value(), bitfield_mask()) -> data().
%% ====================================================================
bit_set(Byte,Value,Mask) ->
	%% Clear the bits specified by MASK first.
	MaskT = bnot(Mask),
	ByteT = Byte band MaskT,
	
	%% Set the required bits.
	ByteT bor Value.

%% ====================================================================
%% @doc
%% This is the same function what bit_set/3 is, but here the input Value
%% does not shifted to the right bit positions, the must do before
%% set the required valus of the bits.
%% @end
-spec bit_set(data(), bitfield_value(), bitfield_mask(), doShiftValueBeforeSet) -> data().
%% ====================================================================
bit_set(Byte,BitFieldValue,BitFieldMask,doShiftValueBeforeSet) ->
	%% Get the list of bit of bitfield_mask.
	BitListOfMask = bit_list_get(BitFieldMask),
	
	%% Find the position of the 1st 1 from right. This will set how 
	%% to shif the Value to left for fit to the bitfield_mask.
	BitPos = find_1st_1_bit_from_right(BitListOfMask),
	
	%% Shift the Value by BitPos to left
	ValueT = BitFieldValue bsl BitPos,
	
	%% Set the bits in the given byte.
	bit_set(Byte, ValueT, BitFieldMask).

%% ====================================================================
%% @doc
%% Clear the bit on the specified position in the byte.
%% TODO: It would be good to compare the length of data with bit,
%%       and reject the operation if bit > than bit length of data. 
%% @end
-spec bit_clear(data(), bit()) -> data().
%% ====================================================================
bit_clear(Byte, Bit) when Byte =< 255, Bit =< 7 ->
	MASK = bnot(1 bsl Bit),
	Byte band MASK.

%% ====================================================================
%% @doc
%% Filter out bits in a byte by the bitfield_mask.
%% @end
-spec bit_get(data(), bitfield_mask()) -> bitfield_value().
%% ====================================================================
bit_get(Byte,Mask) ->
	Byte band Mask.

%% ====================================================================
%% @doc
%% This is the same function what bit_get/2 is, but here the Value given by
%% bit_get/2 must shift left till the first bit in the byte.
%% @end
-spec bit_get(data(), bitfield_mask(), doShiftValueAfterGet) -> bitfield_value().
%% ====================================================================
bit_get(Byte,BitFieldMask,doShiftValueAfterGet) ->
	%% Get the bits in the given byte.
	ValueT = bit_get(Byte, BitFieldMask),
	
	%% Get the list of bit of bitfield_mask.
	BitListOfMask = bit_list_get(BitFieldMask),
	
	%% Find the position of the 1st 1 from right. This will set how 
	%% to shif the Value to left for fit to the bitfield_mask.
	BitPos = find_1st_1_bit_from_right(BitListOfMask),
	
	%% Shift the Value by BitPos to left
	ValueT bsr BitPos.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% Find the position of the 1st 1 from right. This will set how 
%% to shif the Value to left for fit to the bitfield_mask.
%% Input:
%%		BitList	:	list of bit, eq: [0,0,0,1,1,0,0,0]
%% Output:
%%		BitPos	:	0..7
%% ====================================================================
find_1st_1_bit_from_right(BitList) ->
	%% Reverse the bit list.
	find_1st_1_bit_from_right_loop(lists:reverse(BitList),0).

find_1st_1_bit_from_right_loop([],BitPos) ->
	BitPos;
find_1st_1_bit_from_right_loop([1|_T],BitPos) ->
	find_1st_1_bit_from_right_loop([],BitPos);
find_1st_1_bit_from_right_loop([0|T],BitPos) ->
	find_1st_1_bit_from_right_loop(T,BitPos+1).

