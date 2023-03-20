/* MultiSelect.rexx
 * Demonstrates multiselect
 *
 * NOTE: `,' is a continuation character for statements that must be entered
 *       on several source lines!!
 */

NL = '0a'x

call addlib("/libs/rexxreqtools.library", 0, -30, 0)

call rtezrequest("This is an example of a" || NL ||,
                 "multiselect filerequester.", "Show me")

call rtfilerequest(, , "Pick some files", , "rtfi_flags=freqf_multiselect",files)

if files == 1 then
  do
    say "You selected" files.count "files"
    do i=1 to files.count
      say "  file" i || ":" files.i
    end
  end
else
  call rtezrequest("You picked no files.", " No ")
