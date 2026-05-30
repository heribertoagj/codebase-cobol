# 07 — Arquivos e FILE STATUS

Padrao de referencia: **CMRU248C**. Programas batch com arquivos devem integrar **POOL7100** via POL7100C.

## FILE-CONTROL

```cobol
           SELECT NOME-ARQ ASSIGN TO UT-S-NOME-ARQ
               FILE STATUS IS WRK-FS-NOME-ARQ.
```

Uma variavel por arquivo: `WRK-FS-<ddname>` PIC X(002).

## Working-Storage

Contexto da operacao:

```cobol
           77  WRK-ABERTURA                PIC X(013) VALUE ' NA ABERTURA '.
           77  WRK-LEITURA                 PIC X(013) VALUE ' NA LEITURA  '.
           77  WRK-FECHAMENTO              PIC X(013) VALUE 'NO FECHAMENTO'.
```

Mensagem de erro:

```cobol
           01  WRK-ERRO-POOL7100.
               05 FILLER                   PIC X(008) VALUE '** ERRO '.
               05 WRK-OPERACAO             PIC X(013).
               05 FILLER                   PIC X(014) VALUE ' DO ARQUIVO '.
               05 WRK-NOME-ARQUIVO         PIC X(008).
               05 FILLER                   PIC X(018) VALUE ' - FILE STATUS = '.
               05 WRK-FILE-STATUS          PIC X(002).
               05 FILLER                   PIC X(004) VALUE ' **'.
```

## Fluxo por operacao

| Operacao | Passos |
|---|---|
| OPEN | OPEN → MOVE contexto abertura → PERFORM testar FS |
| READ seq. | READ → se FS='10' EOF (nao erro) → senao MOVE leitura → PERFORM testar FS |
| WRITE/REWRITE | WRITE → MOVE contexto escrita → PERFORM testar FS |
| CLOSE | CLOSE → MOVE fechamento → PERFORM testar FS |

## Section de teste FS

```cobol
                  IF  WRK-FS-ARQUIVO NOT EQUAL '00'
                      MOVE WRK-FS-ARQUIVO         TO WRK-FILE-STATUS
                      MOVE 'DDNAME'               TO WRK-NOME-ARQUIVO
                      MOVE WRK-ERRO-POOL7100      TO ERR-TEXTO
                      PERFORM 9999-PROCESSAR-ROTINA-ERRO
                  END-IF
```

## Rotina de erro (9999)

```cobol
                  MOVE PROGRAM-ID               TO ERR-PGM
                  CALL 'POOL7100' USING WRK-BATCH ERRO-AREA SQLCA
                  GOBACK
```

## Checklist (cap. 07)

- [ ] FILE STATUS declarado por arquivo
- [ ] Teste FS apos OPEN/READ/WRITE/CLOSE
- [ ] READ sequencial: '10' = fim, nao aciona erro
- [ ] Demais status ≠ '00' → POOL7100
