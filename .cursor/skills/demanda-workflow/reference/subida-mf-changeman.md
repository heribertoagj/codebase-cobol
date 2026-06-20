# Fase 5b — Subida MF (Changeman)

**Disparo:** usuario informa subida/compilacao, ou ao concluir pre-subida aprovada.

**Pre-requisito:** Fase 5 aprovada (ou usuario confirma subida direta).

## Informacoes obrigatorias (solicitar ao usuario)

Antes de atualizar README e avancar para homologacao, **obter**:

1. **Pacote de subida** — numero Changeman (ex.: `RUCA000752`), members incluidos, status do compile (RC=0).
2. **Pacote de contingencia** — numero Changeman (ex.: `RUCA000753`) com versao **PRD pre-demanda** (baseline em `pgm/analise/`, `cpy/analise/`) para retorno pos-implantacao.
3. **Ticket Jira de promocao** — chave (ex.: `TICKET-85280`) e link browse para promocao dos pacotes entre ambientes (TST / HOM / PRD).
4. **Sequencia projeto Capgemini** — codigo **2021-0765286-5-035** (informar em pacotes Changeman, tickets e implantacao PRD). Registrar em `## Sequencia projeto (Capgemini — subida MF)` no README.

Se o usuario ainda nao criou o pacote de contingencia, orientar a monta-lo com os fontes baseline **antes** da implantacao em PRD.

## Passos

1. Solicitar numeros dos pacotes (subida + contingencia), members, resultado do compile e ticket Jira de promocao.
2. Atualizar README:
   - `## Sequencia projeto (Capgemini — subida MF)` (codigo 2021-0765286-5-035)
   - `## Pacotes changeman` (tabela completa)
   - `## Promocao ambientes (Jira)` (ticket, link, pacotes associados, status)
   - `## Evidencias` (registrar pacotes e ticket)
   - `## Status workflow`: `Compilado Changeman — aguardando homologacao`
3. Marcar checkbox `Pronto para subida` e `Em homologacao` em `## Status`.

## Saida

Ver [templates/saidas-obrigatorias.md](../templates/saidas-obrigatorias.md#fase-5b--subida-mf).
