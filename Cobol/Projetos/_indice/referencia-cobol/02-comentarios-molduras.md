# 02 — Comentarios e molduras

## Tipos de comentario

| Tipo | Uso | Formato |
|---|---|---|
| Moldura principal | DIVISION, bloco inicial | `*================================================================*` |
| Moldura secundaria | SECTION, paragrafo, subarea | `*----------------------------------------------------------------*` |
| Area WS | Separar grupos na Working-Storage | FILLER textual entre tracos |
| Observacao tecnica | Flags, debug, anotacao curta | `***************` linha inteira |
| Regra inline | Contexto em logica | `*` ou `****` alinhado ao fixo |

## Molduras

```cobol
     *================================================================*
      DATA                             DIVISION.
     *================================================================*
```

```cobol
     *----------------------------------------------------------------*
      1200-RECEBER-CONTROLE           SECTION.
     *----------------------------------------------------------------*
```

## Area Working-Storage

```cobol
     *----------------------------------------------------------------*
      01 FILLER                        PIC  X(032)    VALUE
            '*** AREA DE ATRIBUTOS        ***'.
     *----------------------------------------------------------------*
```

## Observacao tecnica

```cobol
*************** VARIAVEL PARA PERMITIR TESTES COM DEBUG
*************** B=BATCH / OUTROS=IMS ONLINE
```

## Regra inline na Procedure Division

```cobol
     *
     ****  TELA PARA CONSISTENCIA INCLUSAO COM CONTRATO RUFI
```

## Regras

- Comentarios em coluna fixa com `*` na area de indicador (coluna 7).
- **Nao** usar `*>` nem comentario inline moderno, salvo se o arquivo ja usar.
- **Nao** misturar estilos de comentario no mesmo programa.
- Comentarios descritivos puros (`CG2605** EXCLUIDO...`) nao precisam de hifen — ver cap. 03.

## Checklist (cap. 02)

- [ ] DIVISION e SECTION com molduras corretas
- [ ] Subareas WS com FILLER descritivo quando aplicavel
- [ ] Estilo de comentario consistente no arquivo
