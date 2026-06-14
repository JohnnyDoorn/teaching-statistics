-- hide-html.lua
function RawBlock(el)
  if FORMAT:match("pdf") and el.format == "html" then
    return pandoc.Null()
  end
end
