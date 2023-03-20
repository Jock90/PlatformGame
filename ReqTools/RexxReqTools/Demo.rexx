/*$VER: Demo.rexx 37.21 (15.8.94)
**
**  $Filename: Demo.rexx $
**  $Version: 37.21 $
**  $Date: 15.8.94 $
**  $Creation: 2.1.93 $
**
**  Demo ARexx script for rexxreqtools.library
**
**  (C) Copyright 1993-1994 Rafael D'Halleweyn
*/

/* NOTE: A Comma at the end of a line is no argument seperator but contituation
 *       character for a statement that continues on the next line!!
 */

NL = '0a'x

call addlib("libs/rexxreqtools.library", 0, -30, 0)

call rtezrequest( "RexxReqTools 1.3 Demo" || NL ||,
                  "~~~~~~~~~~~~~~~~~~~~~" || NL ||,
                  "'rexxreqtools.library' offers several" || NL ||,
                  "different types of requesters:", "Let's see them", , ,
                  "rtez_flags = ezreqf_centertext")


call rtezrequest( "But first you'll have to pick the font" || NL ||, 
                  "that you want to use for this demo.")
call rtfontrequest("Pick font", , , font)
if font then
  fonttag = "rt_font=" || font.name || "/" || font.height
else
  fonttag = ""


/* STRING REQUESTER */

call rtezrequest( "NUMBER 1:" || NL ||,
                  "String requester" || NL ||,
                  "function: rtgetstring()", "Show me" , , fonttag)

buffer = rtgetstring("A bit of text", , "Enter anything:", , fonttag)
if buffer == "" then
  call rtezrequest("You entered nothing!", "I'm sorry", , fonttag)
else
  call rtezrequest( "You entered this string:" || NL ||,
                    "'" || buffer || "'.", "So I did", , fonttag)

buffer = rtgetstring(buffer,,
  "These are two new features of ReqTools 2.0:" || NL ||,
  "Text above the entry gadget and more than" || NL ||,
  "one response gadget.", "Enter anything:",,
  " _Ok |New _2.0 feature!|_Cancel", fonttag)
if rtresult == 2 then call rtezrequest( "Yep, this is a new" || NL ||,
                                        "ReqTools 2.0 feature!", "Oh boy!",,
                                       , fonttag)

call rtgetstring(buffer, "New is also the ability to switch off" || NL ||,
                         "the backfill pattern. You can also center" || NL ||,
                         "the text above the entry gadget." || NL ||,
                         "These new features are also available" || NL ||,
                         "in the rtgetlong() requester.",,
  "Enter anything:"," _Ok |_Abort|_Cancel",,
  fonttag,
  "rtgs_backfill = false rtgs_flags = gsreqf_centertext|gsreqf_highlighttext")
if rtresult == 2 then
  call rtezrequest( "What!! You pressed abort!?!" || NL ||,
                    "You must be joking :-)", "Ok, continue", , fonttag)


/* NUMBER REQUESTER */

call rtezrequest( "NUMBER 2:" || NL ||,
                  "Number requester" || NL ||,
                  "function: rtgetlong()", "Show me", , fonttag)

long = rtgetlong(, , "Enter a number:", , fonttag,
                                          "rtgl_min = -666 rtgl_max = 666")
if rtresult == 0 then
  call rtezrequest("You entered nothing!", "I'm sorry", , fonttag)
else
  do
    text =         "The number you entered was:" || NL
    text = text || long
    if long == 666 then text = text "(you devil!)"
    if long == -666 then text = text "(you negative devil!)"
    call rtezrequest(text, "So it was", , fonttag)
  end


/* MESSAGE REQUESTER */

call rtezrequest( "NUMBER 3:" || NL ||,
                  "Message requester, the requester" || NL ||,
                  "you've been using all the time!" || NL ||,
                  "function: rtezrequest()", "Show me more", , fonttag)

call rtezrequest( "Simplest usage: some body text" || NL ||,
                  "and a single centered gadget.", "Got it", , fonttag)

do while rtezrequest( "You can also use two gadgets" || NL ||,
                      "to ask the user something." || NL ||,
                      "Do you understand?", "Of course|Not really", , fonttag) == 0
  call rtezrequest("You are not one of the brightest are you?",,
        "Let me try again", , fonttag)
end

call rtezrequest("Great, we'll continue then.", "Fine", , fonttag)

call rtezrequest( "You can also put up a requester" || NL ||,
                  "with three choices:" || NL ||,
                  "How do you like the demo so far ?",,
                  "_Great|_So so|_Rubbish", , fonttag)
if rtresult == 0 then
  call rtezrequest( "Too bad, we really hoped" || NL ||,
                    "you would like it better.", "So what", , fonttag)
if rtresult == 1 then
  call rtezrequest("We're glad you like it so much.", "Fine", , fonttag)
if rtresult == 2 then
  call rtezrequest( "Maybe if you run the demo" || NL ||,
                    "again you'll REALLY like it!", "Perhaps", , fonttag)

call rtezrequest( "The number of responses is not limited to" || NL ||,
                  "three as you can see. The gadgets are labeled" || NL ||,
                  "with the return code from rtezrequest()." || NL ||,
                  "Pressing Return will choose 4, note that" || NL ||,
                  "4's button text is printed in boldface.",,
  "1|2|3|4|5|0", , fonttag "rtez_defaultresponse = 4")

call rtezrequest("You picked '" || rtresult || "'.", "How true", , fonttag)

if rtezrequest( "It is also possible to pass extra IDCMP flags" || NL ||,
                "that will satisfy rtezrequest(). This requester" || NL ||,
                "has had 'idcmp_diskinserted' passed to it." || NL ||,
                "(Try inserting a disk).", "Continue", ,,
    fonttag "rt_idcmpflags = idcmp_diskinserted") == 0 then
  call rtezrequest("You used the 'Continue' gadget.", "I did", , fonttag)
else
  call rtezrequest("You inserted a disk.", "I did", , fonttag)

call rtezrequest( "Finally, it is possible to specify the position of the" || NL ||,
                  "requester." || NL ||,
                  "E.g. at the top left of the screen, like this." || NL ||,
                  "This works for all requesters, not just rtezrequest()!",,
  "Amazing", , fonttag "rt_reqpos = reqpos_topleftscr")

call rtezrequest( "Alternatively, you can center" || NL ||,
                  "the requester on the screen." || NL ||,
                  "Check out 'rexxreqtools.guide'" || NL ||,
                  "for all the possibilities.", "I'll do that", ,,
  fonttag "rt_reqpos = reqpos_centerscr")


/* FILE REQUESTER */

call rtezrequest( "NUMBER 4:" || NL ||,
                  "File requester" || NL ||,
                  "function: rtfilerequest()", "Demonstrate", , fonttag)

filename = rtfilerequest(, , "Pick a file" , , fonttag "rtfi_buffer = true")
if rtresult == 0 then
  call rtezrequest("You didn't pick a file.", "No", , fonttag)
else
  call rtezrequest("You picked the file:" || NL || filename, "Right", , fonttag)

call rtezrequest( "The file requester has the ability" || NL ||,
                  "to allow you to pick more than one" || NL ||,
                  "file (use Shift to extend-select)." || NL ||,
                  "Note the extra gadgets you get.", "Interesting", , fonttag)

filename = rtfilerequest(, , "Pick some files", ,,
            fonttag "rtfi_flags = freqf_multiselect")
if rtresult == 1 then
  call rtezrequest( "You selected" rtresult.count "files, " || NL ||,
                    "this is the first one:" || NL ||,
                    rtresult.1, "Aha", , fonttag)

call rtezrequest( "The file requester can be used" || NL ||,
                  "as a directory requester as well.", "Let's see that", ,,
                   fonttag)

dirname = rtfilerequest(, , "Pick a directory", ,,
            fonttag "rtfi_flags = freqf_nofiles")
if rtresult == 0 then
  call rtezrequest("You didn't pick a directory.", "No", , fonttag)
else
  call rtezrequest("You picked the directory:" || NL || dirname, "Right", ,,
        fonttag)


/* FONT REQUESTER */

call rtezrequest( "NUMBER 5:" || NL ||,
                  "Font requester" || NL ||,
                  "function: rtfontrequest()", "Show", , fonttag)

call rtfontrequest("Pick a font", , fonttag "rtfo_flags = freqf_style")
if rtresult == 0 then
  call rtezrequest("You canceled." || NL || "Was there no font you liked ?",,
         "Nope", , fonttag)
else
  call rtezrequest( "You picked the font:" || NL ||,
                    rtresult.name "size" rtresult.height, "Right", , fonttag)


/* PALETTE REQUESTER */

call rtezrequest( "NUMBER 6:" || NL ||,
                  "Palette requester" || NL ||,
                  "function: rtpaletterequest()", "Proceed", , fonttag)

color = rtpaletterequest(, "Change palette", fonttag)
if rtresult == -1 then
  call rtezrequest("You canceled." || NL || "No nice colors to be picked ?",,
        "Nah", , fonttag)
else
  call rtezrequest("You picked color number" color, "Sure did", , fonttag)


/* VOLUME REQUESTER */

call rtezrequest( "NUMBER 7:" || NL ||,
                  "Volume requester" || NL ||,
                  "function: rtfilerequest() with" || NL ||,
                  "          rtfi_volumerequest tag.", "Show me", , fonttag)

volumename = rtfilerequest(, , "Pick a volume", ,,
              fonttag "rtfi_volumerequest = 0")
if rtresult == 0 then
  call rtezrequest("You didn't pick a volume.", "I did not", , fonttag)
else
  call rtezrequest("You picked the volume:" || NL || volumename, "Right", ,,
        fonttag)


/* SCREENMODE REQUESTER */

call rtezrequest( "NUMBER 8:" || NL ||,
                  "Screenmode requester" || NL ||,
                  "function: rtscreenmoderequest().", "Let's have a look", ,,
                  fonttag)
screenmode = rtscreenmoderequest( "Pick screentype", ,,
              fonttag "rtsc_flags = screqf_sizegads|screqf_depthgad", screen)
if screenmode ~= "" then
  call rtezrequest( "So you'd like to open a" || NL ||,
                    screen.name "screen.", "That's right", , fonttag)
else
  call rtezrequest( "Don't you have the right monitor ?", " No ", , fonttag)


call rtezrequest("That's it!" || NL || "Hope you enjoyed the demo",,
      "Sure did", , fonttag)

call rtfreefilebuffer()
