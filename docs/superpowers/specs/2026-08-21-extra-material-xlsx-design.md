# Extra material XLSX import

Builder accepts CSV, TSV, TXT, and XLSX tables for Extra material. Delimited files remain one table per file. Every non-empty XLSX worksheet becomes an independent table named `<workbook> · <sheet>` and uses the existing rename/remove controls. Empty or unreadable sheets are reported without discarding valid sheets. The existing Viewer Extra material table selector selects imported worksheets; no second Viewer sheet control is added. The implementation reuses the existing `readxl` dependency and the existing CRB table attachment path.
