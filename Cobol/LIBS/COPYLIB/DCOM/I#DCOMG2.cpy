      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G2 - DB2PRD.TTRASI_INSTR_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  G2-ARGUMENTOS-ENTRADA.

       05  G2-COLUNAS-DA-TABELA.
           10  G2-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  G2-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  G2-NTITLO-DESC-COML             PIC S9(5)V COMP-3.
           10  G2-NSEQ-INSTR-PCELA             PIC S9(2)V COMP-3.
           10  G2-DINSTR-DESC-PCELA            PIC X(10).
           10  G2-VINSTR-DESC-PCELA            PIC S9(15)V9(2) COMP-3.
           10  G2-PINSTR-DESC-PCELA            PIC S9(3)V9(3) COMP-3.
           10  G2-HULT-ATULZ                   PIC X(26).
           10  G2-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G2-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G2-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G2-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G2-RETORNO.
       05  G2-COD-RETORNO                         PIC 9(04).
       05  G2-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G2-PROXIMO-RESTART                     PIC 9(05).
       05  G2-QTDE-RETORNADA                      PIC 9(03).
       05  G2-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG2.
           10  G2RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  G2RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  G2RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  G2RT-NSEQ-INSTR-PCELA           PIC S9(2)V COMP-3.
           10  G2RT-DINSTR-DESC-PCELA          PIC X(10).
           10  G2RT-VINSTR-DESC-PCELA          PIC S9(15)V9(2) COMP-3.
           10  G2RT-PINSTR-DESC-PCELA          PIC S9(3)V9(3) COMP-3.
           10  G2RT-HULT-ATULZ                 PIC X(26).
           10  G2RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G2RT-CTERM                      PIC X(8).
