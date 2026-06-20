# Fase 5 — Pre-subida

**Disparo:** `Pre-subida <ID>`

**Pre-requisito:** Fase 4b concluida (`Riscos certificados — aguardando pre-subida`). Se `## Certificacao de riscos (pos-desenvolvimento)` ausente ou com veredito **REALIZADO**, orientar `Certificar riscos <ID>` ou retorno a Fase 4.

## Passos

1. Acionar skill `demanda-pre-subida` (gate unificado — padrao cliente; nao fundir com outras validacoes).
2. Corrigir achados **BLOQUEANTE** com `bradesco-quality-code`.
3. Se REPROVADO: nao marcar pronto para subida; listar acoes.
4. Se APROVADO: atualizar Status README.

`## Status workflow`: `Pre-subida aprovada — aguardando subida MF`.

## Saida

Relatorio `demanda-pre-subida` + orientacao: usuario valida e sobe para MF.

## Fase 5b — Subida MF

Ver [reference/subida-mf-changeman.md](../reference/subida-mf-changeman.md).
