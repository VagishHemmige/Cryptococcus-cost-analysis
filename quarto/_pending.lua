-- Marks provisional content in yellow and explains the mark on pages that
-- have it (AGENTS.md, "Visual cues").
--
-- - A paragraph that starts with **Decision pending:** is wrapped in a
--   `pending-note` block, with no change to the page's source.
-- - Any span written as [text]{.pending} is marked where it stands.
-- - A page with at least one mark gets a one-line legend at the top.
--
-- Registered under `filters:` in _quarto.yml. The leading underscore keeps
-- Quarto from treating this file as a page.

local LABEL = "Decision pending:"

local SAMPLE = "Text highlighted like this"
local LEGEND = " is provisional: it depends on a decision the analysis plan " ..
  "has not yet made. To comment on any part of this page, highlight the " ..
  "text and use the Comment button."

local function starts_with_label(para)
  local first = para.content[1]
  return first ~= nil and first.t == "Strong"
    and pandoc.utils.stringify(first) == LABEL
end

function Pandoc(doc)
  if not quarto.doc.is_format("html") then return doc end

  local marked = false

  local blocks = doc.blocks:walk({
    Para = function(para)
      if starts_with_label(para) then
        marked = true
        return pandoc.Div({ para }, pandoc.Attr("", { "pending-note" }))
      end
    end,
    Span = function(span)
      if span.classes:includes("pending") then marked = true end
    end,
  })

  if marked then
    local legend = pandoc.Div(
      { pandoc.Para({
        pandoc.Span({ pandoc.Str(SAMPLE) }, pandoc.Attr("", { "pending" })),
        pandoc.Str(LEGEND),
      }) },
      pandoc.Attr("", { "pending-legend" })
    )
    blocks:insert(1, legend)
  end

  doc.blocks = blocks
  return doc
end
