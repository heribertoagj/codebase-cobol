      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0A6 - DB2PRD.TGARNT_OPER_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  A6-ARGUMENTOS-ENTRADA.

       05  A6-COLUNAS-DA-TABELA.
           10  A6-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  A6-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  A6-CTPO-GARNT                   PIC S9(3)V COMP-3.
           10  A6-HULT-ATULZ                   PIC X(26).
           10  A6-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  A6-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A6-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A6-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A6-RETORNO.
       05  A6-COD-RETORNO                         PIC 9(04).
       05  A6-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A6-PROXIMO-RESTART                     PIC 9(05).
       05  A6-QTDE-RETORNADA                      PIC 9(03).
       05  A6-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TA6.
           10  A6RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  A6RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  A6RT-CTPO-GARNT                 PIC S9(3)V COMP-3.
           10  A6RT-HULT-ATULZ                 PIC X(26).
           10  A6RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  A6RT-CTERM                      PIC X(8).
