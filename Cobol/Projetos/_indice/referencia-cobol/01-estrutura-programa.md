# 01 — Estrutura do programa

## Cabecalho documental

Programas nesta workspace usam cabecalho Bradesco com molduras `*====*`, blocos OBJETIVO, TELAS, INC'S, MODULOS.

```cobol
     *================================================================*
      IDENTIFICATION                  DIVISION.
     *================================================================*

      PROGRAM-ID.     NOMEPROG.
      AUTHOR.         HERIBERTO GIANNASI.
     *================================================================*
     *                    NOME DA CONSULTORIA                          *
     *----------------------------------------------------------------*
     *     PROGRAMA....: NOMEPROG                                      *
     *     PROGRAMADOR.: HERIBERTO GIANNASI                            *
     *     ANALISTA....: HERIBERTO GIANNASI                            *
     *     DATA........: MM/AAAA                                       *
     *----------------------------------------------------------------*
     *     OBJETIVO....: DESCRICAO CURTA DO OBJETIVO                   *
     *----------------------------------------------------------------*
     *     TELAS.......:                                               *
     *     INC'S.......:                                               *
     *     MODULOS.....:                                               *
     *================================================================*
```

- Todo texto em **MAIUSCULAS**.
- Preservar consultoria do fonte existente (`7 C O M M`, `S T E F A N I N I`, etc.).
- Em programa novo: inferir consultoria do modulo ou perguntar ao usuario.

## DIVISION e SECTION

```cobol
     *================================================================*
      DATA                             DIVISION.
     *================================================================*

     *================================================================*
      PROCEDURE                       DIVISION.
     *================================================================*
```

Cada SECTION:

```cobol
     *----------------------------------------------------------------*
      1200-NOME-DA-ROTINA             SECTION.
     *----------------------------------------------------------------*

          ... instrucoes ...

     *----------------------------------------------------------------*
      1200-99-FIM.                    EXIT.
     *----------------------------------------------------------------*
```

### Regras bloqueantes (estrutura)

| Regra | Gate |
|---|---|
| Toda logica executavel dentro de uma `SECTION` | Bloqueante |
| Toda `SECTION` encerra com `NNNN-99-FIM. EXIT.` | Bloqueante |
| Prefixo numerico unico por SECTION | Bloqueante |
| `GO TO` apenas dentro da mesma SECTION | Bloqueante |
| Acionar outra SECTION via `PERFORM` | Obrigatorio |
| Paragrafo com logica fora de SECTION | Proibido |

## Programas novos — SECTIONs padrao

Programas **novos** devem usar este esqueleto minimo na Procedure Division:

| SECTION | Funcao |
|---|---|
| `1000-INICIALIZAR` | Abertura: arquivos, CICS, areas, flags iniciais |
| `2000-PROCESSAR` | Logica principal de negocio |
| `3000-FINALIZAR` | Fechamento: CLOSE, GOBACK, liberacao de recursos |

Fluxo:

```
PERFORM 1000-INICIALIZAR
PERFORM 2000-PROCESSAR
PERFORM 3000-FINALIZAR
GOBACK
```

Exemplo:

```cobol
     *----------------------------------------------------------------*
      1000-INICIALIZAR                 SECTION.
     *----------------------------------------------------------------*

          ... OPEN, INIT ...

     *----------------------------------------------------------------*
      1000-99-FIM.                    EXIT.
     *----------------------------------------------------------------*

     *----------------------------------------------------------------*
      2000-PROCESSAR                   SECTION.
     *----------------------------------------------------------------*

          ... negocio ...

     *----------------------------------------------------------------*
      2000-99-FIM.                    EXIT.
     *----------------------------------------------------------------*

     *----------------------------------------------------------------*
      3000-FINALIZAR                   SECTION.
     *----------------------------------------------------------------*

          ... CLOSE ...

     *----------------------------------------------------------------*
      3000-99-FIM.                    EXIT.
     *----------------------------------------------------------------*
```

Rotinas transversais em programas que precisem:

- `9999-ROTINA-ERRO` — tratamento de erro / POOL7100
- Outras SECTIONs auxiliares com prefixo unico (1100-, 1200-, etc.)

Em **programa legado existente**, preservar numeracao ja adotada; em **programa novo**, preferir 1000/2000/3000.

## Indentacao padrao (colunas 20 e 40)

Aplica-se a instrucoes na **Procedure Division** em trechos **novos**. Em fonte existente, preservar alinhamento do arquivo ao editar trechos adjacentes.

| Elemento | Coluna de inicio |
|---|---|
| Verbo / inicio da instrucao (`MOVE`, `IF`, `PERFORM`, `ADD`, …) | **20** |
| Clausula complementar (`TO`, `FROM`, `AND`, `OR`, operando continuado) | **40** |

### Instrucao simples

```cobol
                  MOVE CAMPO-ORIGEM              TO CAMPO-DESTINO
          col 1   .......... col 20 ............ col 40
```

### Aninhamento (+3 posicoes por nivel)

Cada nivel interno (`IF`, `ELSE`, corpo de `EVALUATE`) avanca **3 colunas** no verbo, mantendo a clausula complementar **+20** em relacao ao verbo **do mesmo nivel**.

```cobol
                  IF  WRK-IND EQUAL 'S'
                     MOVE CAMPO-A                 TO CAMPO-B
                     IF  WRK-CONT GREATER ZEROS
                        ADD 1                      TO WRK-TOTAL
                     END-IF
                  END-IF
```

Referencia de colunas (area B):

| Nivel | Verbo (~col) | Complemento (~col) |
|---|---|---|
| 0 (raiz da SECTION) | 20 | 40 |
| 1 (dentro de IF) | 23 | 43 |
| 2 | 26 | 46 |
| 3 | 29 | 49 |

### Quebra de linha

Quando nao couber na mesma linha, colocar a continuacao na **linha de baixo**, alinhando na coluna complementar (40 ou +20 do verbo aninhado):

```cobol
                  MOVE CAMPO-ORIGEM-MUITO-LONGO
                                       TO CAMPO-DESTINO-MUITO-LONGO
```

```cobol
                  IF  WRK-A EQUAL WRK-B
                  AND WRK-C EQUAL WRK-D
                     MOVE VALOR-A                   TO VALOR-B
                                       OF ESTRUTURA-X
```

```cobol
                  PERFORM 1200-PROCESSAR-ITEM
                      THRU 1200-99-FIM
```

### Data Division

- Niveis `01`, `77`: area principal (colunas 8-11 tipicas COBOL fixo).
- Subordinados `05`, `10`, `15`: avancar progressivamente, alinhamento vertical limpo.
- Ver cap. 02 para FILLER de area.

## Convencoes de nomenclatura

- SECTION: `NNNN-NOME-DESCRITIVO` em maiusculas com hifen.
- Paragrafo interno de laco: `NNNN-10-TOPO`, `NNNN-20-...`
- Fechamento: `NNNN-99-FIM. EXIT.`
- Vocabulario legado: `CONSISTIR`, `OBTER`, `TRATAR`, `FINALIZAR`, `CARREGAR`

## Checklist (cap. 01)

- [ ] Cabecalho Bradesco completo
- [ ] Consultoria coerente com modulo
- [ ] Programa novo: 1000-INICIALIZAR / 2000-PROCESSAR / 3000-FINALIZAR
- [ ] Toda SECTION com `-99-FIM. EXIT.`
- [ ] Trecho novo: verbo ~col 20, complemento ~col 40
- [ ] Aninhado: +3 por nivel; quebra alinhada na coluna complementar
