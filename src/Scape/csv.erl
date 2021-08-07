-module(csv).
-export([read/1]).


read(<<Line/binary>>)->
	BinRecord=binary:split(Line,[<<",">>,<<"\n">>],[global,trim]),
	[parseRecord(H)||H<-BinRecord].


parseRecord(<<H/binary>>)->
	try binary_to_integer(H)
    catch
        error:badarg -> try binary_to_float(H)
        				catch
        					error:badarg->binary_to_list(H)
        				end
    end.