/* FontReq.rexx
 * Demonstrates the fontrequester
 *
 * NOTE: `,' is a continuation character for statements that must be entered
 *       on several source lines!!
 */

call addlib("/libs/rexxreqtools.library", 0, -30, 0)

call rtfontrequest('Pick font',,'rtfo_flags = freqf_style',font)
if font then
  do
    text = 'You picked' font.name 'size' font.height
    if font.bold then text = text || '0A'x || 'You selected bold'
    if font.italic then text = text || '0A'x || 'You selected italic'
    if font.underlined then text = text || '0A'x || 'You selected underlined'
    call rtezrequest(text,'Right')
  end
else
  call rtezrequest('You picked no font',"I'm sorry")
