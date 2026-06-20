# Fase 4b — Certificacao de riscos (pos-desenvolvimento)

**Disparo:** `Certificar riscos <ID>`

**Pre-requisito:** Fase 4 concluida (`Implementacao concluida — aguardando certificacao de riscos`).

## Objetivo

Certificar, com evidencia nos fontes alterados, que os **riscos de codificacao** levantados no README **nao se materializaram** — ou documentar excecao bloqueante antes da pre-subida.

Complementa a Fase 5 (`demanda-pre-subida`): a 4b responde riscos **especificos da demanda**; a 5 valida gates **genericos** Bradesco/SICOR/Hexavision.

## Passos

1. Ler README da demanda e consolidar riscos de codificacao a partir de:
   - `### Riscos` ou `### Riscos do plano` (prioridade)
   - Itens de `## Riscos e atencoes` classificados como codificacao
   - Riscos da analise (Fase 2) ainda nao refletidos no plano, se aplicaveis
2. **Nao duplicar** a mesma preocupacao — uma linha por risco distinto.
3. Classificar cada item:
   - **Codificacao** — verificar nesta fase
   - **Nao codificacao** — marcar `PENDENTE TESTE` (homologacao / aceite Jira / integracao externa)
4. Para cada risco de codificacao, verificar nos fontes alterados (`pgm/`, `cpy/`, `mfs/`, `jcl/` — **nao** `*/analise/`):

| Tipo de risco | Verificacao |
|---|---|
| MOVE / conversao / truncamento | Trecho citado no plano; PIC, grupo REDEFINES, tamanho |
| LENGTH / ACU-POSICAO / offset | `LENGTH OF`, contadores de posicao nos sections afetados |
| Ordem tags / serializacao SICOR | Sequencia no PGM vs XSD / checklist SICOR 5.12 |
| Exclusao copybook / campo | Grep por tag/campo excluido ainda ativo na serializacao |
| Caminhos duplicados no PGM | Section alternativa (ex. msg continuacao) vs fluxo principal |
| SQL / DCLGEN | Divergencia entre EXEC SQL e DCLGEN em `LIBS` |
| Rastreio / verbos comentados | Cap. 03 — prefixo demanda, sem verbos validos comentados |

5. Atribuir veredito por risco de codificacao:

| Veredito | Significado |
|---|---|
| **NAO REALIZADO** | Mitigacao implementada; evidencia no fonte |
| **MITIGADO PARCIAL** | Controlado no codigo; depende de teste MF/homologacao |
| **REALIZADO** | Risco materializado — **bloqueia** pre-subida |
| **N/A** | Removido do escopo ou supersedido por decisao tecnica documentada |
| **PENDENTE TESTE** | Risco nao codificacao — delegar a Fase 6 / `## Plano de testes` |

6. Preencher `## Certificacao de riscos (pos-desenvolvimento)` no README (tabela com origem, veredito, evidencia).
7. Emitir saida padronizada — ver [templates/saidas-obrigatorias.md](../templates/saidas-obrigatorias.md#fase-4b--certificacao-de-riscos).
8. Atualizar `## Status workflow`:
   - APROVADO ou APROVADO COM RESSALVAS: `Riscos certificados — aguardando pre-subida`
   - REPROVADO: `Certificacao de riscos reprovada — corrigir implementacao`

## Criterio de aprovacao

| Resultado | Condicao |
|---|---|
| **REPROVADO** | Qualquer risco de codificacao com veredito **REALIZADO** |
| **APROVADO COM RESSALVAS** | Apenas **MITIGADO PARCIAL** em riscos de codificacao (documentado) |
| **APROVADO** | Todos os riscos de codificacao = **NAO REALIZADO** ou **N/A** |

## Regras

- **Nao alterar fontes** nesta fase — somente README e relatorio. Se risco **REALIZADO**, orientar retorno a Fase 4 (`Implementar plano <ID>`).
- Evidencia minima por item: member + section/trecho ou resultado de grep — nao basta afirmacao generica.
- Riscos com mitigacao "validar em teste" sem verificacao estatica possivel: **MITIGADO PARCIAL** ou **PENDENTE TESTE**, conforme natureza.

## Saida

Ver [templates/saidas-obrigatorias.md](../templates/saidas-obrigatorias.md#fase-4b--certificacao-de-riscos).

**Proximo passo:** `Pre-subida <ID>` (Fase 5 — somente apos certificacao aprovada).
