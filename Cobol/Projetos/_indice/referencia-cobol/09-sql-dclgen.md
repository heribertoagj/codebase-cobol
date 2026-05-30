# 09 — SQL e DCLGEN

## Fonte da verdade

O **DCLGEN** (`EXEC SQL DECLARE ... TABLE` no `.cpy` em `LIBS/DCLGEN/`) define o nome real da tabela DB2.

**Nao** inferir tabela a partir de:

- Nome da estrutura host (`RUECB00C`)
- Nome de section ou rotina
- Rotulo de negocio

## Antes de sugerir ou editar EXEC SQL

1. Localizar DCLGEN usado (`EXEC SQL INCLUDE nome` no programa).
2. Confirmar `DECLARE schema.tabela TABLE` no copybook.
3. Validar host variables contra colunas do DCLGEN.

## Divergencia SQL vs DCLGEN

Se `SELECT/INSERT/UPDATE/DELETE` referencia tabela diferente do DCLGEN relacionado → **BLOQUEANTE**.

Exemplo documentado nesta workspace:

- `RUECB00C` → `DB2PRD.TCDULA_TEMPR_RENEG` (`LIBS/DCLGEN/RUEC/ruecb00c.cpy`)

## Rule automatica

Cursor rule: `.cursor/rules/dclgen-sql-validation.mdc` (aplica em `.cbl`/`.cpy`).

## Checklist (cap. 09)

- [ ] DCLGEN localizado em LIBS
- [ ] Nome de tabela confere com SQL
- [ ] Host variables e indicadores alinhados ao DCLGEN
- [ ] Pendencias LIBS justificadas no README
