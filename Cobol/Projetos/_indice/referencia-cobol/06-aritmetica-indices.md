# 06 — Aritmetica e indices

## Operacoes aritmeticas

Campos em `ADD`, `SUBTRACT`, `MULTIPLY`, `DIVIDE`, `COMPUTE` devem estar em formato de **processamento** (compactacao quando exigida pela regra local).

| Risco | Gate |
|---|---|
| Campo de exibicao usado direto em conta | Bloqueante |
| COMPUTE com PIC incompativel | Bloqueante |

Preferir areas de trabalho numericas dedicadas; mover para exibicao apenas na saida.

## Indices e OCCURS

| Risco | Gate |
|---|---|
| Indice / subscript sem compactacao quando exigido | Bloqueante |
| Reutilizar campo de exibicao como indice | Bloqueante |

Usar variavel dedicada para indexacao, coerente com a tabela `OCCURS`.

## Exemplo

```cobol
           77  WRK-INDICE                  PIC  9(003) COMP.
                  COMPUTE WRK-TOTAL = WRK-TOTAL + WRK-VALOR
                  MOVE WRK-INDICE                 TO WRK-IDX-TABELA
```

## Checklist (cap. 06)

- [ ] Contas usam campos de processamento adequados
- [ ] Indices compactados / dedicados
- [ ] Sem campo apenas receptor sem consumo (ver cap. 10 / quality #8)
