# Referencia COBOL Bradesco (desenvolvimento e gate)

Manual padrao desta workspace para **desenvolvimento** (Fase 4), **certificacao de riscos** (Fase 4b) e **gate de qualidade** (Fase 5 / Hexavision).

## Quando consultar

| Momento | Capitulos |
|---|---|
| Programa novo | 01, 02, 04 |
| Alteracao em fonte existente | 03, 04, 05, 08 |
| Copybook / layout SICOR | 03, 08 + `agroleg-knowledge/initiatives/SICOR-5.12.md` |
| Batch com arquivos | 07 |
| SQL / DB2 | 09 |
| Pre-subida / Hexavision | 10 (checklist completo) |

## Indice

| Cap. | Arquivo | Conteudo |
|---|---|---|
| 01 | [01-estrutura-programa.md](01-estrutura-programa.md) | SECTION, 1000/2000/3000, indentacao col 20/40 |
| 02 | [02-comentarios-molduras.md](02-comentarios-molduras.md) | Molduras, FILLER, areas WS |
| 03 | [03-rastreio-alteracao.md](03-rastreio-alteracao.md) | CGaamm*, comentar+nova linha, verbos inviaveis |
| 04 | [04-condicionais-fluxo.md](04-condicionais-fluxo.md) | IF, EVALUATE, GO TO, PERFORM |
| 05 | [05-move-conversao-campos.md](05-move-conversao-campos.md) | PIC, sinal, tamanho, REDEFINES |
| 06 | [06-aritmetica-indices.md](06-aritmetica-indices.md) | COMPUTE, compactacao, OCCURS |
| 07 | [07-arquivos-file-status.md](07-arquivos-file-status.md) | FILE STATUS, POOL7100 (CMRU248C) |
| 08 | [08-copybook-data-division.md](08-copybook-data-division.md) | Exclusao hierarquica, layout COR/CIR |
| 09 | [09-sql-dclgen.md](09-sql-dclgen.md) | EXEC SQL, DCLGEN, tabelas |
| 10 | [10-checklist-gate.md](10-checklist-gate.md) | Checklist unificado pre-subida |

## Programas de referencia

- Estrutura / estilo: `RUCA7951`, `RUEC7951`
- REDEFINES sinal: `RUCA24XC`
- FILE STATUS: `CMRU248C`

## Skills relacionadas

- Estilo: `bradesco-style`
- Quality: `bradesco-quality-code`
- Pre-subida: `demanda-pre-subida`
- Fluxo AGROLEG: `demanda-workflow`, `Projetos/_indice/guia-fluxo-agroleg.md`

## Prefixo de rastreio

Capgemini: prefixo **CGaamm** (`CG` + ano + mes; ex.: `CG2605*` em maio/2026). Outras consultorias: prefixo da entrega em curso. Ver cap. 03.
