%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This module contains functions for convert inter to BCD
%% BCD to integer.
%% @end

-module(bcd).

%% ====================================================================
%% API functions
%% ====================================================================
-export([encode/2, decode/2, decode/3]).

%% ====================================================================
%% @doc
%% Pack the digits of an integer as BCD in a given size of binary.
%% The pad with leading zeros.
%% example:
%% 		47> bcd:encode(19,1).
%% 		&lt;&lt;25&gt;&gt;
%% 		48> 
%% @end
-spec encode(integer(), integer()) -> binary().
%% ====================================================================
encode(IntToBcd, Size) ->
    << <<X:4>> || X <- lists:flatten(io_lib:fwrite("~*..0B", [Size*2, IntToBcd])) >>.

%% ====================================================================
%% @doc
%% Unpack the given size of BCD binary into an integer
%% strip leading zeros.
%% example:
%% 		48> bcd:decode(&lt;&lt;25&gt;&gt;,1).
%% 		19
%% 		49>
%% @end
-spec decode(binary(), integer()) -> integer().
%% ====================================================================
decode(BcdBinary, Size) ->
    decode(BcdBinary, Size, <<>>).

decode(_, 0, BcdToInt) ->
    erlang:binary_to_integer(BcdToInt);
decode(<<BcdBinary_1:4, BcdBinary_2:4, Num/binary>>, Size, BcdToInt) ->
    decode(Num, Size-1, <<BcdToInt/binary, (BcdBinary_1+16#30), (BcdBinary_2+16#30)>>).

%% ====================================================================
%% Internal functions
%% ====================================================================


