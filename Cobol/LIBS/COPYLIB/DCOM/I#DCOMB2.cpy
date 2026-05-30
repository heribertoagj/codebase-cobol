      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B2 - DB2PRD.THIST_GARNT_OPER       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  B2-ARGUMENTOS-ENTRADA.

       05  B2-COLUNAS-DA-TABELA.
           10  B2-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  B2-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  B2-CTPO-GARNT                   PIC S9(3)V COMP-3.
           10  B2-HULT-ATULZ                   PIC X(26).
           10  B2-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  B2-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B2-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B2-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B2-RETORNO.
       05  B2-COD-RETORNO                         PIC 9(04).
       05  B2-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B2-PROXIMO-RESTART                     PIC 9(05).
       05  B2-QTDE-RETORNADA                      PIC 9(03).
       05  B2-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB2.
           10  B2RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  B2RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  B2RT-CTPO-GARNT                 PIC S9(3)V COMP-3.
           10  B2RT-HULT-ATULZ                 PIC X(26).
           10  B2RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  B2RT-CTERM                      PIC X(8).
