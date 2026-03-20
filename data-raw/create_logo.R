# data-raw/create_logo.R
# Reproducible generation of the Vasper hex logo (SVG)
#
# Design concept
# ──────────────
#   • Green hex — soil / agriculture identity
#   • Bot head (center) — AI assistant
#   • Seedling antenna — curved stem with leaf-shaped receivers + signal arcs
#   • Soil-profile layers (base) — soil health data
#   • Small sun (upper-right) — weather
#   • "VASPER" text (bottom) — app name
#
# Run from the project root:
#   source("data-raw/create_logo.R")

library(glue)
library(yaml)

# ── Brand colours ─────────────────────────────────────────────
brand <- read_yaml("_brand.yml")
pal <- brand$color$palette

green <- pal[["vasper-green"]]
orange <- pal[["harvest-orange"]]
fg <- pal[["neutral-fg"]]

# ── Hex geometry ──────────────────────────────────────────────
# Pointy-top regular hexagon.
# viewBox 0 0 200 231  (width 200, height ≈ 200 × 2/√3)
# Vertices inset 3 px so the stroke stays inside the viewBox.
hex <- "100,3 197,59 197,172 100,228 3,172 3,59"

# ── Build SVG ─────────────────────────────────────────────────
svg <- glue(
      '
<svg xmlns="http://www.w3.org/2000/svg"
     viewBox="0 0 200 231" width="200" height="231">

  <defs>
    <clipPath id="hex-clip">
      <polygon points="{hex}"/>
    </clipPath>
  </defs>

  <!-- ── hex background (fill only; border drawn last so it sits on top) ── -->
  <polygon points="{hex}" fill="{green}"/>

  <!-- ── grass & soil profile (clipped to hex) ── -->
  <g clip-path="url(#hex-clip)">
    <!-- B horizon — subsoil (drawn first, bottom-most) -->
    <path d="M0,207 Q40,203 80,208 Q120,213 160,206 Q180,203 200,207
             L200,231 L0,231 Z"
          fill="#8D6E63" opacity="0.85"/>
    <!-- A horizon — dark topsoil -->
    <path d="M0,186 Q45,182 90,187 Q130,192 170,185 Q185,183 200,186
             L200,209 Q180,205 160,209 Q120,215 80,210 Q40,205 0,209 Z"
          fill="#5D4037" opacity="0.85"/>
    <!-- O horizon — thin organic / decomposition layer -->
    <path d="M0,174 Q35,171 70,175 Q105,179 140,174 Q170,171 200,174
             L200,188 Q170,184 140,188 Q105,192 70,187 Q35,184 0,188 Z"
          fill="#3E2723" opacity="0.80"/>
    <!-- grass turf — irregular top edge for organic feel -->
    <path d="M0,167 Q30,161 60,166 Q80,160 100,166
             Q120,160 140,165 Q170,162 200,167
             L200,178 Q150,175 100,178 Q50,181 0,177 Z"
          fill="#388E3C" opacity="0.85"/>

    <!-- subtle roots in O & A horizons -->
    <g stroke="#6D4C41" stroke-width="0.8" fill="none" opacity="0.40">
      <path d="M85,176 Q82,185 78,195 Q76,200 80,208"/>
      <path d="M115,176 Q118,188 120,200"/>
      <path d="M60,177 Q55,190 58,205"/>
      <path d="M140,175 Q145,186 143,198"/>
    </g>
  </g>

  <!-- ── seedling antenna (compact stem + leaf receivers + signal tip) ── -->
  <!-- stem — slight organic curve -->
  <path d="M100,85 C99,72 101,55 100,38"
        stroke="white" stroke-width="2.5" fill="none"
        stroke-linecap="round" opacity="0.90"/>

  <!-- antenna tip / seed bud -->
  <circle cx="100" cy="36" r="3.5" fill="{orange}" opacity="0.90"/>
  <!-- signal arcs (reinforce antenna concept) -->
  <path d="M94,30 Q91,23 93,17"
        stroke="{orange}" stroke-width="1.2" fill="none"
        stroke-linecap="round" opacity="0.45"/>
  <path d="M106,30 Q109,23 107,17"
        stroke="{orange}" stroke-width="1.2" fill="none"
        stroke-linecap="round" opacity="0.45"/>

  <!-- upper leaf pair — pointed, upward-reaching -->
  <path d="M100,50 C97,48 91,42 88,44 C85,46 96,52 100,52 Z"
        fill="white" opacity="0.85"/>
  <path d="M100,50 C103,48 109,42 112,44 C115,46 104,52 100,52 Z"
        fill="white" opacity="0.85"/>

  <!-- lower leaf pair — broader, angled out -->
  <path d="M100,67 C97,65 90,59 87,61 C84,63 96,69 100,69 Z"
        fill="white" opacity="0.70"/>
  <path d="M100,67 C103,65 110,59 113,61 C116,63 104,69 100,69 Z"
        fill="white" opacity="0.70"/>

  <!-- subtle leaf veins -->
  <line x1="100" y1="51" x2="90" y2="43"
        stroke="{green}" stroke-width="0.6" opacity="0.25"/>
  <line x1="100" y1="51" x2="110" y2="43"
        stroke="{green}" stroke-width="0.6" opacity="0.25"/>

  <!-- ── bot head ── -->
  <!-- subtle drop shadow -->
  <rect x="76" y="88" width="52" height="44" rx="9" ry="9"
        fill="{fg}" opacity="0.12"/>
  <!-- face plate -->
  <rect x="74" y="85" width="52" height="44" rx="9" ry="9"
        fill="white" opacity="0.95"/>

  <!-- eyes -->
  <circle cx="89"  cy="101" r="5.5" fill="{green}"/>
  <circle cx="111" cy="101" r="5.5" fill="{green}"/>

  <!-- eye highlights -->
  <circle cx="91"  cy="99" r="1.8" fill="white"/>
  <circle cx="113" cy="99" r="1.8" fill="white"/>

  <!-- mouth (subtle smile) -->
  <path d="M92,118 Q100,124 108,118"
        stroke="{green}" stroke-width="2.2" fill="none"
        stroke-linecap="round"/>

  <!-- ── sun (upper-right) ── -->
  <circle cx="152" cy="65" r="9" fill="{orange}" opacity="0.70"/>
  <g stroke="{orange}" stroke-width="1.5"
     stroke-linecap="round" opacity="0.50">
    <!-- 8 rays: N, NE, E, SE, S, SW, W, NW -->
    <line x1="152" y1="51" x2="152" y2="54"/>
    <line x1="161" y1="56" x2="159" y2="58"/>
    <line x1="166" y1="65" x2="163" y2="65"/>
    <line x1="161" y1="74" x2="159" y2="72"/>
    <line x1="152" y1="79" x2="152" y2="76"/>
    <line x1="143" y1="74" x2="145" y2="72"/>
    <line x1="138" y1="65" x2="141" y2="65"/>
    <line x1="143" y1="56" x2="145" y2="58"/>
  </g>

  <!-- ── app name (between bot head and soil profile) ── -->
  <text x="100" y="153"
        text-anchor="middle"
        font-family="system-ui, -apple-system, sans-serif"
        font-weight="700" font-size="19" fill="white"
        letter-spacing="1">VASPER</text>

  <!-- ── hex border (drawn last so it sits on top of all layers) ── -->
  <polygon points="{hex}"
           fill="none" stroke="{fg}" stroke-width="3"/>

</svg>
'
)

# ── Write ─────────────────────────────────────────────────────
out_path <- file.path("www", "icons", "vasper-logo.svg")
writeLines(svg, out_path)
message(glue("Logo saved to {out_path}"))
