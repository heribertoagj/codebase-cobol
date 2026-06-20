# Fase 4 — Desenvolvimento

**Disparo:** `Implementar plano <ID>`

**Pre-requisito:** plano validado explicitamente pelo usuario (Fase 3).

## Passos

1. Executar plano na ordem definida em `## Plano de ajuste (proposta)`.
2. Acionar skills filhas conforme escopo — ver tabela em [SKILL.md](../SKILL.md):
   - Sempre: `bradesco-style`, `bradesco-quality-code`
   - Condicional: SICOR, CICS, SQL, batch
3. **Padrao cliente — rastreio (nao simplificar):** comentar linha antiga + criar linha nova; verbos comentados com `-`; **nao excluir linhas** — ver `referencia-cobol/03-rastreio-alteracao.md` e `bradesco-quality-code`.
4. Programa novo: 1000-INICIALIZAR / 2000-PROCESSAR / 3000-FINALIZAR (`referencia-cobol/01-estrutura-programa.md`).
5. Indentacao trecho novo: verbo col 20, complemento col 40.
6. Atualizar README: `## Decisoes tecnicas`, `## Impacto tecnico (confirmado)`.
7. Auto-verificacao cap. 03: prefixo da demanda atual (nao historico) — `bradesco-quality-code` secao "Prefixo historico vs demanda atual".
8. `## Status workflow`: `Implementacao concluida — aguardando certificacao de riscos`.

## Saida

Ver [templates/saidas-obrigatorias.md](../templates/saidas-obrigatorias.md#fase-4--implementacao).

**Proximo passo:** `Certificar riscos <ID>` (Fase 4b — antes da pre-subida).
