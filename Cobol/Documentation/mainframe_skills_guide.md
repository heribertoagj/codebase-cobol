# Guia de Aperfeiçoamento em Mainframe (COBOL, CICS, JCL, IMS, DB2)

Este guia foi elaborado para elevar o nível de desenvolvimento no ambiente Mainframe, focando em melhores práticas, performance e recursos avançados.

## 1. JCL & Utilities (DFSORT/ICETOOL)

O JCL é a linguagem de controle, mas o poder real está nos utilitários. O uso avançado do DFSORT pode eliminar a necessidade de muitos programas COBOL simples.

### DFSORT Avançado
*   **OUTFIL**: Use para gerar múltiplos arquivos numa única passagem, filtrar registros, formatar relatórios e converter dados (como visto no seu trabalho recente).
    ```jcl
    OUTFIL FNAMES=SAIDA1,INCLUDE=(11,15,CH,EQ,C'000...')
    OUTFIL FNAMES=SAIDA2,SAVE
    ```
*   **IFTHEN**: Permite lógica condicional complexa dentro do SORT/COPY.
    ```jcl
    INREC IFTHEN=(WHEN=(1,2,CH,EQ,C'AA'),OVERLAY=(10:C'TIPO A')),
          IFTHEN=(WHEN=NONE,OVERLAY=(10:C'OUTROS'))
    ```
*   **JOINKEYS**: O "Join do SQL" para arquivos sequenciais/VSAM. Substitui programas de "Balance Line". Muito mais performático que fazer match em COBOL.
*   **SYMBOLS**: Use símbolos para não chumbar posições de colunas no JCL.
    ```jcl
    //SYMNAMES DD *
    CNPJ-POS,11,15
    /*
    //SYSIN DD *
    SORT FIELDS=(CNPJ-POS,CH,A)
    ```

### Dicas de JCL
*   **RESTART**: Use `RESTART=STEPxx` no cartão JOB para continuar de um ponto em caso de falha, mas garanta que os datasets criados nos steps falhos sejam deletados/catalogados corretamente.
*   **COND**: Prefira `IF/THEN/ELSE/ENDIF` no lugar do parâmetro `COND` nos steps, pois é mais legível.

## 2. COBOL Moderno e Performance

Embora o COBOL seja antigo, o compilador Enterprise COBOL traz otimizações modernas.

*   **Tipos de Dados**:
    *   Prefira `BINARY` (ou `COMP`) para contadores e índices.
    *   Use `COMP-3` (Packed Decimal) para valores monetários e campos que interagem com DB2 `DECIMAL`.
*   **Indexação vs Subscripting**:
    *   `TABELA(INDICE)` é mais rápido que `TABELA(CONTADOR)` porque o índice já contém o deslocamento de memória. Use `INDEXED BY`.
*   **Intrinsic Functions**: Use funções nativas para evitar rotinas complexas.
    *   `FUNCTION CURRENT-DATE`
    *   `FUNCTION NUMVAL-C` (converte string monetária para número)
    *   `FUNCTION UPPER-CASE` / `LOWER-CASE`
*   **EVALUATE vs IF**: Use `EVALUATE TRUE` para condições complexas. É mais fácil de ler e manter.
    ```cobol
    EVALUATE TRUE
       WHEN TIPO-CLIENTE = 1 AND SALDO > 1000
          PERFORM TRATA-VIP
       WHEN TIPO-CLIENTE = 2
          PERFORM TRATA-COMUM
       WHEN OTHER
          PERFORM TRATA-ERRO
    END-EVALUATE
    ```

## 3. DB2 Performance & Coding

O gargalo da maioria dos programas Batch/Online é o I/O de banco de dados.

*   **SARGable Predicates**: Escreva cláusulas `WHERE` que permitam ao DB2 usar índices (Stage 1).
    *   *Evite*: `WHERE YEAR(DATA) = 2024` (Isso força table space scan).
    *   *Prefira*: `WHERE DATA BETWEEN '2024-01-01' AND '2024-12-31'`.
*   **Multi-Row Fetch**: Em programas Batch, use `FETCH ROWSET` para ler 100 ou 1000 linhas de uma vez, reduzindo as trocas de contexto entre o programa e o DB2.
    ```cobol
    EXEC SQL FETCH NEXT ROWSET FROM CURSOR1 FOR 100 ROWS INTO :VETOR-ESTRUTURA END-EXEC.
    ```
*   **Commit Frequency**: Em batch, faça COMMIT a cada N registros (ex: 1000 ou 5000) para evitar travar recursos (Locks), mas não a cada registro (overhead alto).
*   **Error Handling**: Sempre verifique `SQLCODE`.
    *   `0`: Sucesso.
    *   `+100`: Fim de cursor ou registro não encontrado.
    *   `-803`: Violação de chave única.
    *   `-911/-913`: Deadlock/Timeout (necessário lógica de retry).

## 4. CICS Best Practices (Online)

O ambiente online exige tempos de resposta sub-segundo.

*   **Pseudo-Conversacional**: Garanta que o programa termine e devolva o controle ao CICS enquanto o usuário digita na tela (`EXEC CICS RETURN TRANSID(...) COMMAREA(...)`).
*   **COMMAREA vs Channels/Containers**:
    *   `COMMAREA` é limitada a 32KB.
    *   Para dados maiores ou estruturas mais flexíveis, use **Channels e Containers**. É a forma moderna de passar dados entre programas.
*   **Tratamento de Exceções**:
    *   Use `RESP(WS-RESP)` em todos os comandos CICS e trate o retorno. Evite `HANDLE CONDITION` (é considerado obsoleto e "spaghetti code").
*   **Thread Safety**: Se estiver rodando em ambiente com OTE (Open Transaction Environment), garanta que o programa seja Thread-safe para evitar trocas para o QR TCB.

## 5. IMS (Hierarchical DB)

Se estiver trabalhando com IMS DB (DL/I):

*   **SSAs (Segment Search Arguments)**: Construa SSAs qualificados para ir direto ao registro. SSAs não qualificados forçam leitura sequencial (muito lento).
*   **Status Codes**:
    *   `GE`: Not found.
    *   `GB`: End of database.
    *   `II`: Segment already exists (duplicidade).
*   **Checkpoint/Restart**: Em programas BMP (Batch Message Processing) longos, use `CHKP` periódicos para liberar recursos e `XRST` para permitir restart.

## Sugestão de Estudo Prático

1.  **Refatoração**: Pegue um programa antigo que usa `IF` aninhados e refatore para `EVALUATE`.
2.  **Otimização DB2**: Analise um JCL de carga pesada e veja se o cursor pode usar `WITH UR` (Uncommitted Read) se a integridade permitir, ou implemente `Multi-Row Fetch`.
3.  **Modernização JCL**: Substitua um step de programa COBOL que apenas filtra/formata arquivo por um step de `ICETOOL` ou `SORT`.

Se precisar de exemplos específicos de código para qualquer um desses tópicos, posso gerar para você!
