---
name: bradesco-quality-code
description: Gate Hexavision para fontes COBOL Bradesco — rastreio de alteracao, MOVE, aritmetica, FILE STATUS, copybook. Use em revisao, subida, promover ou corrigir apontamentos. Referencia completa em Cobol/Projetos/_indice/referencia-cobol/.
---

# Bradesco Quality Code

## Objetivo

Aplicar regras **bloqueantes** de quality code (Hexavision) antes de subida.

**Documentacao completa:** `Cobol/Projetos/_indice/referencia-cobol/`

| Capitulo | Regra |
|---|---|
| [03-rastreio-alteracao.md](../../../Cobol/Projetos/_indice/referencia-cobol/03-rastreio-alteracao.md) | Comentar+nova linha; verbos `-`; **nao excluir** |
| [05-move-conversao-campos.md](../../../Cobol/Projetos/_indice/referencia-cobol/05-move-conversao-campos.md) | PIC, sinal, tamanho |
| [06-aritmetica-indices.md](../../../Cobol/Projetos/_indice/referencia-cobol/06-aritmetica-indices.md) | COMPUTE, OCCURS |
| [07-arquivos-file-status.md](../../../Cobol/Projetos/_indice/referencia-cobol/07-arquivos-file-status.md) | POOL7100 / CMRU248C |
| [08-copybook-data-division.md](../../../Cobol/Projetos/_indice/referencia-cobol/08-copybook-data-division.md) | Hierarquia excluida |
| [10-checklist-gate.md](../../../Cobol/Projetos/_indice/referencia-cobol/10-checklist-gate.md) | Gate completo |

## Regras bloqueantes (indice)

| # | Regra | Cap. |
|---|---|---|
| 1 | Sem comandos validos comentados; alteracao = comentar + nova linha | 03 |
| 2 | Sem MOVE entre formatos incompativeis | 05 |
| 3 | Sem MOVE com destino menor (truncamento) | 05 |
| 4 | Sem aritmetica sem compactacao quando exigida | 06 |
| 5 | Sem indice de lista nao compactado | 06 |
| 6 | Sem MOVE sinalizado → nao sinalizado sem REDEFINES | 05 |
| 7 | FILE STATUS + POOL7100 em programas com arquivos | 07 |
| 8 | Sem variavel elementar apenas receptora | 10 |
| 9 | SICOR 5.12: ver iniciativa no acervo | 08 + SICOR-5.12.md |
| 10 | Exclusao copybook: arvore completa comentada | 08 |

## Regra principal

1. Tratar regras como **bloqueantes** salvo instrucao explicita em contrario.
2. Validar PIC, tamanho, sinal antes de aceitar MOVE.
3. **Nunca excluir linha** em `.cbl`/`.cpy` — comentar com prefixo da demanda e criar linha nova (cap. 03).

## Prefixo de rastreio (Capgemini)

Formato: **CGaamm**

| Parte | Significado |
|---|---|
| `CG` | Capgemini |
| `aa` | Dois ultimos digitos do ano corrente |
| `mm` | Mes corrente (01–12) |

Exemplo em maio/2026: **`CG2605`**. Usar o prefixo do mes/ano da entrega em curso (`CG2605*`, `CG2605**`, `CG2605` em linha ativa nova).

Outras consultorias: prefixo proprio da entrega (`STE001*`, `7C5.10*`, etc.) — ver cap. 03.

## Como reportar achados

```text
BLOQUEANTE: [cap. NN / regra #N]
Trecho encontrado: ...
Risco: ...
Correcao recomendada: ...
```

## Combinar com

- Estilo visual: `bradesco-style`
- SICOR 5.12: `agroleg-knowledge/initiatives/SICOR-5.12.md`
- Pre-subida: `demanda-pre-subida`
- SQL: cap. 09 + rule `dclgen-sql-validation`

## Heuristicas rapidas

- `CGaamm*` (ex.: `CG2605*`) + verbo valido (`IF`, `MOVE`...) **sem** `-` → violacao imediata #1
- **Qualquer** prefixo historico comentado nesta demanda (`STE001*`, `BRQ001*`, etc.) em vez do prefixo **atual** → violacao rastreio
- PGM comentado com prefixo que existe **so no copy** do campo destino → suspeita imediata (ver secao abaixo)
- Linha apagada em vez de comentada → violacao #1 / rastreio
- Diferenca de tamanho ou sinal em MOVE → suspeita imediata
- `01` comentado com `05` ativo → violacao #10

## Prefixo historico vs demanda atual (cap. 03)

Linhas **ativas** antigas podem ter prefixo de outra entrega (`STE001`, `BRQ001`, `7C5.10`, `CG0523`).
Ao **comentar** essa linha na demanda **em curso**, usar **sempre** o prefixo da entrega atual (`CG2605*`, etc.).

**Nunca** transformar `STE001` em `STE001*` para registrar alteracao de AGROLEG-1482.
**Nunca** copiar o prefixo do copybook para o PGM so porque o campo tem o mesmo nome.

### Anti-padrao (BLOQUEANTE)

```cobol
STE001*    MOVE  ORIGEM  TO  CAMPO-ANTIGO        ← prefixo errado + MOVE sem '-'
```

### Padrao correto — substituir destino, mesma origem (SICOR)

```cobol
CG2605*    -MOVE  ORIGEM-X13  TO  CAMPO-FLAT-X13
CG2605     MOVE  ORIGEM-X13  TO  GRUPO-X13
```

Copybook e PGM sao rastreados **independentemente** (mesmo prefixo CG2605, invalidacao conforme Procedure ou Data Division).

### Auto-verificacao pos-edicao (Fase 4)

Antes de marcar implementacao concluida, buscar em fontes alterados:

- linhas `*MOVE`, `*IF`, `*PERFORM` sem `-` apos o prefixo de comentario
- linhas comentadas nesta demanda com prefixo diferente do da entrega
- diff vs baseline: linha ativa virou comentada com prefixo historico errado?
