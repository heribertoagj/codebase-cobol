# 03 — Rastreio de alteracao

Aplica-se a **alteracoes em programas (`.cbl`) e copybooks (`.cpy`)**. Objetivo: rastrear o que mudou, passar no Hexavision e preservar auditoria.

## Principios obrigatorios

1. **Nao excluir linhas** — comentar a linha original e **criar linha nova** com a alteracao.
2. **Identificar** cada linha comentada com prefixo da demanda (ex.: `CG2605*` para Capgemini em maio/2026).
3. **Nunca** deixar comandos COBOL **validos** em linhas comentadas — invalidar verbos com `-`.
4. Incluir cabecalho descritivo em blocos grandes (exclusoes, migracao SICOR).

## Prefixo de rastreio

### Capgemini (AGROLEG)

Formato: **CGaamm**

| Parte | Significado |
|---|---|
| `CG` | Capgemini |
| `aa` | Dois ultimos digitos do ano corrente |
| `mm` | Mes corrente (01–12) |

Exemplo em maio/2026: **`CG2605`**. O prefixo reflete o mes/ano da **entrega em curso**, nao o numero da historia Jira.

Formato na coluna de comentario:

```cobol
CG2605*    ... linha comentada com verbo inviavel ...
CG2605**   ... texto descritivo puro (sem verbo COBOL) ...
CG2605     ... linha ativa nova (sem asterisco apos o prefixo) ...
```

### Outras consultorias

Usar prefixo da entrega em curso (ex.: `STE001*` Stefanini, `7C5.10*` 7Comm).

## Fluxo padrao — Procedure Division

### Substituir instrucao

**Antes (linha ativa):**
```cobol
                  MOVE CAMPO-ANTIGO              TO CAMPO-DESTINO
```

**Depois:**
```cobol
CG2605*    -MOVE CAMPO-ANTIGO              TO CAMPO-DESTINO
                  MOVE CAMPO-NOVO                 TO CAMPO-DESTINO
```

### Substituir condicional

**Antes:**
```cobol
                  IF  WRK-IND EQUAL 'S'
                     MOVE CAMPO-A                 TO CAMPO-B
                  END-IF
```

**Depois:**
```cobol
CG2605** EXCLUIDA REGRA LEGADA - SICOR 5.12
CG2605*    -IF  WRK-IND EQUAL 'S'
CG2605*        -MOVE CAMPO-A                 TO CAMPO-B
CG2605*    END-IF
                  IF  WRK-IND-NEW EQUAL 'S'
                     MOVE CAMPO-C                 TO CAMPO-B
                  END-IF
```

## Verbos que devem receber `-` quando comentados

Invalidar **todos** os verbos-reserva do bloco:

`IF`, `MOVE`, `ADD`, `SUBTRACT`, `MULTIPLY`, `DIVIDE`, `COMPUTE`, `PERFORM`, `CALL`, `READ`, `WRITE`, `OPEN`, `CLOSE`, `SELECT`, `GO`, `EVALUATE`, `EXEC SQL`, `EXEC CICS`, `INCLUDE`

## Linhas que podem ficar sem `-`

- Cabecalhos descritivos: `CG2605** EXCLUIDO GRUPO...`
- Fechamentos estruturais sem acao: `END-IF`, `END-EXEC`, `END-EVALUATE` (quando nao forem verbos de acao)

## Data Division (copybook)

### Substituir definicao

```cobol
CG2605** EXCLUIDO GRUPO CERTIFICACAO ORGANICA - SICOR 5.12
CG2605* -01 GRUPO-CERTIF-ORGNC.
CG2605*    -05 CAMPO-A                   PIC X(010).
           01 GRUPO-NOVO.
              05 CAMPO-B                 PIC X(020).
```

### Exclusao sem substituicao imediata

Comentar **toda a arvore** (`01` + subordinados) — ver cap. 08.

```cobol
CG2605* -01 GRUPO-EXCLUIDO.
CG2605*    -05 CAMPO-A                   PIC X(010).
CG2605*    -05 CAMPO-B                   PIC 9(005).
```

Invalidar niveis: `-01`, `-05`, `-PIC` quando aplicavel.

## SQL / listas comentadas

Em blocos `EXEC SQL` ou listas `USING` comentadas, prefixar identificadores ou linhas com `-` para evitar sintaxe reativavel.

## O que NAO fazer

| Errado | Motivo |
|---|---|
| Apagar linha e so incluir a nova | Perde rastreio e auditoria |
| `CG2605*    IF condicao` (verbo valido) | **BLOQUEANTE** Hexavision |
| `STE001*    MOVE origem TO destino-antigo` nesta demanda | Prefixo historico no comentario — usar `CG2605* -MOVE` |
| Comentar `01` e deixar `05` filho ativo | Orfaos na Data Division |
| Alterar sem prefixo de demanda | Sem rastreio |
| Copiar prefixo do copybook para PGM ao comentar MOVE | Prefixos sao independentes por fonte |

## Prefixo historico vs demanda atual

Linhas **ativas** antigas podem ter prefixo de outra entrega (`STE001`, `BRQ001`, `CG0523`).
Ao comentar na demanda **em curso**, usar prefixo **atual** (`CG2605*`) com verbos/niveis invalidados (`-MOVE`, `-05`).

**Exemplo — PGM (troca de destino SistProdc, mesma origem X(13)):**

Baseline ativo:
```cobol
                  MOVE  RUEC58-SIST-PROD      TO  RUCA66-COD-SIST-PRODC
```

Correto (AGROLEG-1482 / CG2605):
```cobol
CG2605*    -MOVE  RUEC58-SIST-PROD      TO  RUCA66-COD-SIST-PRODC
CG2605     MOVE  RUEC58-SIST-PROD      TO  RUCA66-GP-COR0001-SIST-PRODC
```

**Invalido (confundiu prefixo STE001 do copy com rastreio do PGM):**
```cobol
STE001*    MOVE  RUEC58-SIST-PROD      TO  RUCA66-COD-SIST-PRODC
```

**Exemplo — copybook (campo STE001 ativo substituido por grupo):**
```cobol
CG2605** SUBSTITUIDO COD-SIST-PRODC POR GRUPO SISTPRODC - SICOR 5.12
CG2605*    -05 RUCA66-COD-SIST-PRODC        PIC  X(013) VALUE SPACES.
CG2605     05 RUCA66-GP-COR0001-SIST-PRODC.
```

## Exemplo invalido vs valido (Hexavision)

**Invalido:**
```cobol
CG2605*    IF  WRK-NULL LESS ZEROS
CG2605*        MOVE SPACES              TO CAMPO-TEXT
CG2605*    END-IF.
```

**Valido:**
```cobol
CG2605*    -IF  WRK-NULL LESS ZEROS
CG2605*        -MOVE SPACES              TO CAMPO-TEXT
CG2605*    END-IF.
```

## Checklist (cap. 03)

- [ ] Nenhuma linha excluida — comentada + nova linha criada
- [ ] Prefixo de demanda em linhas comentadas
- [ ] Verbos comentados com `-`
- [ ] Cabecalho descritivo em blocos de exclusao
- [ ] Nenhuma referencia ativa a campo/logica excluida
- [ ] Copybook: hierarquia completa comentada (cap. 08)
