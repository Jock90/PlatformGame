/* Buffered.rexx
 * Demonstrates buffered filerequesters
 *
 * NOTE: `,' is a continuation character for statements that must be entered
 *       on several source lines!!
 */

NL = '0a'x

call addlib("/libs/rexxreqtools.library", 0, -30, 0)

call rtezrequest("This is an example of" || NL ||,
                 "buffered filerequesters.", "Show me")

call rtfilerequest(, , "Pick a file", , "rtfi_buffer = true")

call rtezrequest("If we now use the filerequester" || NL ||,
                 "again it will have remembered" || NL ||,
                 "the last directories contents.", "Show me more")

call rtfilerequest(, , "Pick another file")

if ~rtezrequest("Now, if you started this script from a Shell and if you" || NL ||,
                "start this script again from the same Shell, the file-" || NL ||,
                "requester will remember the last directories contents" || NL ||,
                "inbetween executions of this script.",,
                "I'll try it|I've just tried it") then
  call rtfreefilebuffer()
