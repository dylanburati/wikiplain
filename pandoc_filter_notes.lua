function Pandoc(doc)
    local notes = {}
    for _, el in pairs(doc.blocks) do
        if (el.t == "Div" and el.attributes.role == "note") then
            table.insert(notes, el)
        elseif (el.t == "Header" and el.level > 1 and el.level <= 3) then
            break
        end
    end
    return pandoc.Pandoc(notes, doc.meta)
end
