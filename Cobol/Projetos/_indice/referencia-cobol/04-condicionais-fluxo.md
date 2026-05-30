# 04 — Condicionais e fluxo

## IF / EVALUATE

- Declarar dentro de `SECTION` (cap. 01).
- Indentacao: verbo ~col 20; aninhado +3 por nivel (cap. 01).
- Condicoes compostas: `AND` / `OR` alinhados; quebrar linha na coluna complementar se necessario.

```cobol
                  IF  WRK-A EQUAL WRK-B
                  AND WRK-C EQUAL WRK-D
                     MOVE VALOR                      TO DESTINO
                  END-IF
```

## GO TO

| Permitido | Proibido |
|---|---|
| Desvio dentro da **mesma** SECTION | `GO TO` para paragrafo de **outra** SECTION |
| Loop: `GO TO NNNN-10-TOPO` | Pular logica de outro modulo via GO TO |
| Saida antecipada: `GO TO NNNN-99-FIM` | |

Exemplo de laco:

```cobol
     *----------------------------------------------------------------*
      1051-PROCESSAR-LACO             SECTION.
     *----------------------------------------------------------------*

      1051-10-TOPO.

                  IF  WRK-CONT GREATER WRK-LIMITE
                      GO                      TO 1051-99-FIM

                  ...
                  ADD 1                       TO WRK-CONT
                  GO                          TO 1051-10-TOPO

     *----------------------------------------------------------------*
      1051-99-FIM.                    EXIT.
     *----------------------------------------------------------------*
```

## PERFORM

- Acionar outra SECTION: `PERFORM NOME-SECTION`
- Intervalo dentro da mesma SECTION: `PERFORM PARRA THRU PARRA-FIM`
- **Nao** usar GO TO para substituir PERFORM entre SECTIONs.

## Null indicators (SQL)

Antes de usar campo com possivel NULL, validar indicador:

```cobol
                  IF  WRK-CAMPO-NULL LESS ZEROS
                     MOVE CAMPO-DB                   TO DESTINO
                  END-IF
```

Ao excluir logica com null indicator, aplicar cap. 03 (comentar + nova linha, verbos `-`).

## Checklist (cap. 04)

- [ ] IF/EVALUATE dentro de SECTION
- [ ] GO TO restrito a mesma SECTION
- [ ] Outras SECTIONs via PERFORM
- [ ] Indentacao col 20/40 em trecho novo
