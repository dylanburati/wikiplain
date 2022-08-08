function Superscript(el)
    all_citenotes = true
    for _, dchild in pairs(el.content) do
        child = dchild
        if (dchild.t == "Emph") and (#dchild.content == 1) then
            child = dchild.content[1]
        end

        if not (child.t == "Space") and
           not (child.t == "LineBreak") and
           not (child.t == "SoftBreak") and
           not (child.t == "Str" and
                ((child.text == "[") or (child.text == "]"))) and
           not ((child.t == "Link") and
                (("#" == string.sub(child.target, 1, 1)) or
                 ("/wiki/Wikipedia:Citation_needed" == child.target) or
                 ("?action=edit" == string.sub(child.target, #child.target - 11))))
        then
            all_citenotes = false
            -- return pandoc.Span(child, {debug = "line10"})
            break
        end
    end
    if all_citenotes then return {} end
    return el
end

function Link(el)
    if (el.classes[1] == "image") and
       (#el.content == 1) and
       (el.content[1].t == "Image")
    then
        el.content[1].src = el.target
        return el.content[1]
    end
    return el
end

function Span(el)
    if (#el.content == 1) and
       (el.content[1].t == "Str") and
       not (string.find(el.content[1].text, "[^%s]"))
    then
        return {}
    end
    return el
end

function Pandoc(doc)
    doc2 = doc:walk {
        Div = function (el)
            local function has_stuff(ch)
                return (ch.t ~= "Space") and
                    (ch.t ~= "LineBreak") and
                    (ch.t ~= "SoftBreak")
            end
            if (not el.content:find(has_stuff)) or
               (el.classes:includes("sidebar")) or
               (el.classes:includes("shortdescription")) or
               (el.attributes.role == "note")
            then
                return {}
            end
            pandoc.Para(el.content)
        end,

        Table = function (el)
            local function notarticle(class_name)
                return string.sub(class_name, 1, 4) == "box-"
            end
            if (el.classes:find(notarticle))
            then
                return {}
            end
            return el
        end
    }
    return doc2
end
