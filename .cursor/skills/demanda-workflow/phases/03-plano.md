# Fase 3 — Plano de ajuste

**Disparo:** `Gerar plano de ajuste <ID>`

**Pre-requisito:** Fase 2 concluida (`ANALISE CONCLUIDA`). Se README ainda tiver ambiguidades criticas de negocio, sugerir `Clarificar requisitos <ID>` antes de montar o plano.

## Passos

1. Com base na analise, montar `## Plano de ajuste (proposta)` no README.
2. Incluir ordem de implementacao, riscos e cenarios de teste preliminares.
3. Referenciar regras aplicaveis via skills filhas (SICOR 5.12, quality code, style).

## Formato do plano (README)

```markdown
## Plano de ajuste (proposta)
| # | Fonte | Area/Section | Alteracao proposta | Referencia |
| 1 | ... | ... | ... | SICOR 5.12 / quality #10 / ... |

### Ordem de implementacao
1. ...

### Riscos
- ...

### Cenarios de teste (preliminar)
- ...
```

## Saida e gate

- Saida: [templates/saidas-obrigatorias.md](../templates/saidas-obrigatorias.md#fase-3--plano-de-ajuste)
- `## Status workflow`: `Plano proposto — aguardando validacao`
- **Gate (padrao cliente):** implementacao **somente** apos usuario dizer `Implementar plano <ID>`.
