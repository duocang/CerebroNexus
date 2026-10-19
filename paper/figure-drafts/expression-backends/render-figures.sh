#!/usr/bin/env bash
# Render the expression-backend figures for Genome Biology with Arial embedded,
# then audit every PDF: fonts embedded and Arial only, width 170 mm, height
# <= 225 mm. Run inside the project Nix shell from the repository root:
#
#   nix-shell default.nix -A shell --run \
#     'bash paper/figure-drafts/expression-backends/render-figures.sh'
#
# Arial is taken from ARIAL_DIR, or from the first standard location holding
# arial.ttf (Windows fonts seen from WSL, Linux msttcorefonts, macOS). Only that
# directory is exposed to fontconfig, so no other family can be substituted.
# Extra arguments are passed to rebuild-figures.R.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
repo="$(cd "$here/../../.." && pwd -P)"

font_src=""
for dir in "${ARIAL_DIR:-}" /mnt/c/Windows/Fonts /usr/share/fonts/truetype/msttcorefonts \
  /usr/share/fonts/msttcore-fonts /Library/Fonts /System/Library/Fonts/Supplemental; do
  if [ -n "$dir" ] && find "$dir" -maxdepth 1 -iname 'arial.ttf' 2>/dev/null | grep -q .; then
    font_src="$dir"
    break
  fi
done
if [ -z "$font_src" ]; then
  echo "Arial not found; set ARIAL_DIR to a directory containing arial.ttf" >&2
  exit 1
fi

fonts="$(mktemp -d)"
trap '[ -n "$fonts" ] && rm -rf -- "$fonts"' EXIT
find "$font_src" -maxdepth 1 -iname 'arial*.ttf' -exec cp {} "$fonts/" \;
cat > "$fonts/fonts.conf" <<CONF
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <dir>$fonts</dir>
  <cachedir>$fonts/cache</cachedir>
</fontconfig>
CONF
export FONTCONFIG_FILE="$fonts/fonts.conf"
echo "Arial from: $font_src"

out_dir="$repo/paper/figure-drafts/expression-backends"
for arg in "$@"; do
  case "$arg" in --out=*) out_dir="${arg#--out=}" ;; esac
done

Rscript "$here/rebuild-figures.R" "$repo" "$@"

if ! command -v pdffonts >/dev/null || ! command -v pdfinfo >/dev/null; then
  echo "pdffonts/pdfinfo unavailable; PDF audit skipped" >&2
  exit 0
fi
status=0
for pdf in "$out_dir"/fig-*-expression-backends.pdf; do
  [ -f "$pdf" ] || continue
  # Fonts: every row embedded, every face an Arial face.
  fonts_report="$(pdffonts "$pdf" | tail -n +3)"
  if [ -z "$fonts_report" ] || echo "$fonts_report" | awk '{ if ($(NF-4) != "yes") bad = 1 } END { exit !bad }' \
     || echo "$fonts_report" | awk '$1 !~ /Arial/ { bad = 1 } END { exit !bad }'; then
    echo "FAIL fonts $pdf" >&2
    pdffonts "$pdf" >&2
    status=1
  fi
  # Size: 170 mm wide (481.9 pt), at most 225 mm tall (637.8 pt).
  size="$(pdfinfo "$pdf" | awk '/^Page size:/ { print $3, $5 }')"
  if ! echo "$size" | awk '{ exit !($1 >= 481.5 && $1 <= 482.5 && $2 <= 637.8) }'; then
    echo "FAIL size $pdf: $size pt" >&2
    status=1
  fi
  echo "audited $(basename "$pdf"): $(echo "$size" | awk '{ printf "%.1f x %.1f mm", $1 / 72 * 25.4, $2 / 72 * 25.4 }'), fonts: $(echo "$fonts_report" | awk '{ print $1 }' | sort -u | tr '\n' ' ')"
done
exit "$status"
