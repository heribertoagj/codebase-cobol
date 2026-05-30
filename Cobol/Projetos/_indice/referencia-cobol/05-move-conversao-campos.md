# 05 — MOVE e conversao de campos

## Regra geral

Antes de aceitar `MOVE`, comparar origem e destino: `PIC`, tamanho, decimais, sinal, compactacao.

## Formato diferente — BLOQUEANTE

Incompatibilidades tipicas:

- Alfanumerico → numerico sem tratamento
- Numerico editado → numerico de processamento
- Campo com sinal → campo de outra natureza
- Exibicao → compactado sem conversao explicita

## PIC X → PIC 9 — padrao obrigatorio

**Nao** fazer `MOVE campo-pic-x TO campo-pic-9` direto.

1. Validar com `IS NUMERIC` quando aplicavel.
2. Area de grupo sem PIC no nivel 01; campo numerico subordinado.

```cobol
           01  WRK-9-9-X.
               05  WRK-9-9                 PIC  9(009).

                  IF  CAMPO-X IS NUMERIC
                     MOVE CAMPO-X                 TO WRK-9-9-X
                     MOVE WRK-9-9                 TO CAMPO-NUMERICO
                  END-IF
```

## Tamanho diferente — destino menor — BLOQUEANTE

- Nao aceitar truncamento implicito.
- Validar inteiros, decimais, posicao de sinal.
- Ajustar destino ou tratar valor antes do MOVE.

## Campo com sinal → sem sinal — REDEFINES (RUCA24XC)

**Nao** mover assinado para nao assinado sem area intermediaria.

```cobol
           01  WRK-S9-09                   PIC +9(009)         VALUE ZEROS.
           01  FILLER                      REDEFINES WRK-S9-09.
               05 WRK-9-09                 PIC  9(009).
```

Fluxo: mover para `WRK-S9-xx` (assinado), usar `WRK-xx` (nao assinado) no proximo MOVE.

## Alteracoes

Ao substituir MOVE existente: cap. 03 — comentar linha antiga com `-MOVE`, criar linha nova.

## Checklist (cap. 05)

- [ ] Sem MOVE entre formatos incompativeis
- [ ] PIC X → PIC 9 via grupo + IS NUMERIC
- [ ] Sem truncamento implicito
- [ ] Sinal tratado via REDEFINES quando necessario
