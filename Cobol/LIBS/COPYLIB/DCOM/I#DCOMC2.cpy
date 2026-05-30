      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C2 - DB2PRD.TINSTR_DESC_PCELA      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  C2-ARGUMENTOS-ENTRADA.

       05  C2-COLUNAS-DA-TABELA.
           10  C2-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  C2-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  C2-NTITLO-DESC-COML             PIC S9(5)V COMP-3.
           10  C2-NSEQ-INSTR-PCELA             PIC S9(2)V COMP-3.
           10  C2-DINSTR-DESC-PCELA            PIC X(10).
           10  C2-VINSTR-DESC-PCELA            PIC S9(15)V9(2) COMP-3.
           10  C2-PINSTR-DESC-PCELA            PIC S9(3)V9(3) COMP-3.
           10  C2-HULT-ATULZ                   PIC X(26).
           10  C2-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  C2-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C2-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C2-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C2-RETORNO.
       05  C2-COD-RETORNO                         PIC 9(04).
       05  C2-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C2-PROXIMO-RESTART                     PIC 9(05).
       05  C2-QTDE-RETORNADA                      PIC 9(03).
       05  C2-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC2.
           10  C2RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  C2RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  C2RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  C2RT-NSEQ-INSTR-PCELA           PIC S9(2)V COMP-3.
           10  C2RT-DINSTR-DESC-PCELA          PIC X(10).
           10  C2RT-VINSTR-DESC-PCELA          PIC S9(15)V9(2) COMP-3.
           10  C2RT-PINSTR-DESC-PCELA          PIC S9(3)V9(3) COMP-3.
           10  C2RT-HULT-ATULZ                 PIC X(26).
           10  C2RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  C2RT-CTERM                      PIC X(8).
