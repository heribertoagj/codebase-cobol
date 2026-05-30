      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B3 - DB2PRD.THIST_INSTR_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  B3-ARGUMENTOS-ENTRADA.

       05  B3-COLUNAS-DA-TABELA.
           10  B3-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  B3-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  B3-NSEQ-INSTR-PCELA             PIC S9(2)V COMP-3.
           10  B3-NTITLO-DESC-COML             PIC S9(5)V COMP-3.
           10  B3-HULT-ATULZ                   PIC X(26).
           10  B3-DINSTR-DESC-PCELA            PIC X(10).
           10  B3-VINSTR-DESC-PCELA            PIC S9(15)V9(2) COMP-3.
           10  B3-PINSTR-DESC-PCELA            PIC S9(3)V9(3) COMP-3.
           10  B3-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  B3-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B3-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B3-RETORNO.
       05  B3-COD-RETORNO                         PIC 9(04).
       05  B3-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B3-PROXIMO-RESTART                     PIC 9(05).
       05  B3-QTDE-RETORNADA                      PIC 9(03).
       05  B3-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB3.
           10  B3RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  B3RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  B3RT-NSEQ-INSTR-PCELA           PIC S9(2)V COMP-3.
           10  B3RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  B3RT-HULT-ATULZ                 PIC X(26).
           10  B3RT-DINSTR-DESC-PCELA          PIC X(10).
           10  B3RT-VINSTR-DESC-PCELA          PIC S9(15)V9(2) COMP-3.
           10  B3RT-PINSTR-DESC-PCELA          PIC S9(3)V9(3) COMP-3.
           10  B3RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  B3RT-CTERM                      PIC X(8).
