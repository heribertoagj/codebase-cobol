# Copilot Instructions — 7CO-25-9999-01

Purpose: Help AI coding agents be immediately productive working on the BVVE/DCOM COBOL migration repository (CNPJ numeric → alphanumeric).

Key places to read first
- `prompt.txt` — authoritative migration rules (CNPJ/FILIAL changes, DB2 rules, formatting). Start here.
- `BVVE/pgm/` and `DCOM/pgm/` — main COBOL programs to modify (`*.cbl`).
- `BVVE/cpy/` and `TABELAS/` — copybooks and table definitions used by programs.

High-level project goals (what changes are allowed)
- Migrate CNPJ-related fields from numeric PIC 9(...) to alphanumeric PIC X(...). Only fields whose NAMES contain `CPF-CNPJ`, `CGC`, or `CNPJ` are in-scope.
- Migrate `FILIAL` fields from PIC 9(05) (or numeric) to PIC X(04). When in a file FD record, add a `FILLER X(1)` if needed to preserve record length.
- Preserve existing behavior; do not apply unrelated refactorings.

Formatting and coding conventions the agent must follow
- Prefix for new/inserted lines: `7C2511`. If the file already uses another prefix for previous changes, replace that prefix with `7C2511` on the new lines.
- Comment-out old code rather than delete; keep original lines but commented with `*` in column 7 (standard COBOL comment column).

Prefix & column alignment (strict)
- **Prefix position:** `7C2511` MUST start in columns 1–6 (no leading spaces). Column 7 is reserved for the COBOL comment marker: use `7C2511*` for commented original lines (the `*` must be in column 7).
- **Working-Storage alignment:** level `01` entries start at column 8 (one space after column 7). All lower-level entries (`05`, `10`, `77`, etc.) start at column 20 (Area B). Example:

```
7C2511 01  WRK-AREA              (level-01 starts at col 8)
7C2511             05  WRK-FIELD-1   PIC X(09)  (level-05 starts at col 20)
7C2511             10  WRK-FIELD-2   PIC X(04)
```

- **Procedure Division alignment:** statements (MOVE, CALL, IF, PERFORM) must begin at column 20. When adding prefixed active code use `7C2511` then pad so the COBOL statement starts in column 20. Example:

```
7C2511             MOVE ZEROS TO WRK-CAD-CONTROLE.
7C2511             CALL WRK-BRAD2000 USING WRK-CAD-CPFCGC
7C2511                        WRK-CAD-CONTROLE.
```

Post-edit validation (PowerShell)
- Use these commands to find lines where the `7C2511` prefix is not placed at column 1 (i.e. there are leading spaces before it). Run from the repo root in PowerShell:

```powershell
Get-ChildItem -Recurse -Include *.cbl | Select-String -Pattern '7C2511' | ForEach-Object {
  $idx = $_.Line.IndexOf('7C2511')
  if ($idx -ne 0) { Write-Output "$($_.Path):$($_.LineNumber): idx=$idx  $($_.Line)" }
}
```

- To quickly normalize (move `7C2511` to column 1) for a single file, use this example script (review before running):

```powershell

Variable naming and movement patterns
- When adding temporary / staging fields, use the `-ST` suffix (examples found in `prompt.txt`):
  - `CCGC-CPF` → `CCGC-CPF-ST`
  - `CFLIAL-CGC` → `CFLIAL-CGC-ST`
  - `CCTRL-CPF-CGC` → `CCTRL-CPF-CGC-ST`
```

Note: the normalization script is a blunt tool — review diffs carefully. The agent should prefer to manually align inserted lines when possible and run the validation commands afterwards.
- For calls to the digit-checker utility, replace `BRAD0110` calls with `BRAD2000`. Use the WRK-BRAD2000 calling pattern from `prompt.txt`, moving the numeric/alphanumeric fields into `WRK-CAD-NUMERO` and `WRK-CAD-FILIAL` before call and checking `RETURN-CODE` afterwards.

PIC changes (exact rule)
- Names containing `CPF-CNPJ`, `CGC`, or `CNPJ`: change from `PIC 9(09)` (or similar numeric) to `PIC X(09)` (or same length X). If code uses `9(08)`/`9(09)` preserve the same length when converting to `X(...)`.
- Names containing `FILIAL`: change from `PIC 9(05)` to `PIC X(04)`. If the field exists inside an FD record, append a `FILLER X(1)` to keep record length constant.

DB2 / embedded SQL rules (discoverable patterns)
- For affected tables (see `Stefanini/TABELAS` referenced in `prompt.txt`) update SELECT/INSERT/UPDATE to add new fields with `-ST` suffix and keep existing attributes. Do not remove existing fields.
- In `WHERE` clauses: comment out old field references and add the `-ST` field instead (preserve original clauses as commented lines). Same for `DELETE` and `FETCH INTO` clauses: ensure `INTO`/`SELECT` target list matches.
- Only edit DB2 access for tables listed in the provided TABELAS folder (do not modify other tables).

Safety rules (hard constraints)
- Do not modify files outside `7CO-25-9999-01` workspace or outside the directories documented in `prompt.txt`.
- Only change variables whose names contain the exact substrings: `CPF-CNPJ`, `CGC`, `CNPJ`, `FILIAL`.
- Never apply additional refactorings, style changes, or logic improvements beyond the specified migration.

Examples (from repository rules)
- Procedure call pattern (use exactly):
```
7C2511     MOVE ZEROS          TO WRK-CAD-CONTROLE.
7C2511     MOVE WRK-CGC-CPF    TO WRK-CAD-NUMERO.
7C2511     MOVE WRK-FILIAL     TO WRK-CAD-FILIAL.
7C2511     CALL WRK-BRAD2000  USING WRK-CAD-CPFCGC
7C2511                        WRK-CAD-CONTROLE.
```

What to look for while editing a program
- Instances of `PIC 9(` with variable names containing the target substrings; update to `PIC X(` preserving lengths.
- FD records in `BVVE/pgm` and `DCOM/pgm`: if `FILIAL` exists, add `FILLER X(1)` at end of that record to keep layout.
- Copybook includes in `COPY` statements referencing BVVE copybooks — prefer local `BVVE/cpy/` copies. If you must change a copybook that belongs to the BVVE library, copy it from `C:\Users\herib\OneDrive\Documents\Workspace\Cobol\LIBS\COPYLIB\BVVE` into `BVVE/cpy/` and edit the copy in `BVVE/cpy/` only.

Missing or non-discoverable items
- No build/test scripts or GitHub Actions were detected; assume COBOL toolchain is external. Ask a human for their compile/test commands before attempting automated runs.

When done
- Add a short checklist inside the modified COBOL file (near the top) using the project's prefix `7C2511` with bullets showing:
  - Correção formato de variaveis CNPJ
  - Copybook corrigido (if applicable)
  - Correção na consistências das variaveis
  - Correção de acessos DB2 (if applicable)

If anything is unclear, ask a human before changing more than three files in one commit.

---
If you want, I can now: (1) generate the file patch I just added, (2) scan `BVVE/pgm` for the first candidate program to modify, or (3) create a small checklist generator to insert into modified files. Which should I do next?
