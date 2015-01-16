#!/usr/local/bin/escript
%%! -smp enable -sname unpacket

-mode(compile).  % for better performance

-define(IPV4, 4).
-define(IP_MIN_HDR_LEN, 5).
-define(TCP_MIN_HDR_LEN, 5).


main([]) ->
    %% Packet - example from http://bluegraybox.com/blog/2013/04/27/unpacking-packets/
    %%     4500 0041 d2b4 4000 4006 6a00 7f00 0001
    %%     7f00 0001 e7dc abcd 22f4 9ff5 a016 2056
    %%     8018 0101 fe35 0000 0101 080a 02e5 5833
    %%     02e5 4d46 6865 6c6c 6f20 776f 726c 6421
    %%     0a
    %% 65 bytes, 520 bits total
    % Dgram = <<16#45000041d2b4400040066a007f0000017f000001e7dcabcd22f49ff5a016205680180101fe3500000101080a02e5583302e54d4668656c6c6f20776f726c64210a:520>>,
    Dgram = <<
        16#45000041d2b4400040066a007f000001:128,
        16#7f000001e7dcabcd22f49ff5a0162056:128,
        16#80180101fe3500000101080a02e55833:128,
        16#02e54d4668656c6c6f20776f726c6421:128,
        16#0a:16>>,

    DgramSize = size(Dgram),
    case Dgram of
        <<
            ?IPV4:4, HLen:4, SrvcType:8, TotLen:16,
            ID:16, Flgs:3, FragOff:13,
            TTL:8, Proto:8, HdrChkSum:16,
            SrcIP:32, DestIP:32, 
            RestDgram/binary
        >> when HLen >= ?IP_MIN_HDR_LEN, 4*HLen =< DgramSize ->
            OptsLen = 4*(HLen - ?IP_MIN_HDR_LEN),
            <<Opts:OptsLen/binary, TCPPacket/binary>> = RestDgram
    end,
    <<SrcIPa:8, SrcIPb:8, SrcIPc:8, SrcIPd:8>> = <<SrcIP:32>>,
    <<DestIPa:8, DestIPb:8, DestIPc:8, DestIPd:8>> = <<DestIP:32>>,

    case TCPPacket of
        <<
            SrcPort:16, DestPort:16, SeqNum:32, AckNum:32,
            DataOff:4, Reserved:3, Flags:9, WinSize:16, ChkSum:16, Urg:16,
            RestPacket/binary
        >> ->
            TCPOptsLen = 4*(DataOff - ?TCP_MIN_HDR_LEN),
            <<TCPOpts:TCPOptsLen/binary, Data/binary>> = RestPacket
    end,
    io:format("HLen: ~w, SrvcType: ~w, TotLen: ~w,~n" ++
        "ID: ~w, Flgs: ~w, FragOff: ~w,~n" ++
        "TTL: ~w, Proto: ~w, HdrChkSum: ~w,~n" ++
        "SrcIP: ~w.~w.~w.~w,~n" ++
        "DestIP: ~w.~w.~w.~w, ~n" ++
        "Opts: ~w~n",
        [HLen, SrvcType, TotLen, ID, Flgs, FragOff, TTL, Proto, HdrChkSum, SrcIPa, SrcIPb, SrcIPc, SrcIPd, DestIPa, DestIPb, DestIPc, DestIPd, Opts]),
    io:format("SrcPort: ~w (~.16b), DestPort: ~w (~.16b),~n" ++
        "SeqNum: ~w,~n" ++
        "AckNum: ~w,~n" ++
        "DataOff: ~w, Reserved: ~w, Flags: ~w, WinSize: ~w,~n" ++
        "ChkSum: ~w, Urg: ~w,~n" ++
        "TCPOpts: ~w,~n" ++
        "Data: '~s'~n",
        [SrcPort, SrcPort, DestPort, DestPort, SeqNum, AckNum, DataOff, Reserved, Flags, WinSize, ChkSum, Urg, TCPOpts, Data]).
