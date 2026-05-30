      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB093 - DB2PRD.TARGMT_SOLTC_RELAT     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  93-ARGUMENTOS-ENTRADA.

       05  93-COLUNAS-DA-TABELA.
           10  93-CSOLTC-RELAT-DESC            PIC S9(9)V COMP-3.
           10  93-CARGMT-RELAT-DESC            PIC S9(3)V COMP-3.
           10  93-NSEQ-ARGMT-SOLCT             PIC S9(3)V COMP-3.
           10  93-RARGMT-SOLTC-RELAT           PIC X(30).
           10  93-HULT-ATULZ                   PIC X(26).
           10  93-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  93-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  93-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  93-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  93-RETORNO.
       05  93-COD-RETORNO                         PIC 9(04).
       05  93-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  93-PROXIMO-RESTART                     PIC 9(05).
       05  93-QTDE-RETORNADA                      PIC 9(03).
       05  93-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  93-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T93.
           10  93RT-CSOLTC-RELAT-DESC          PIC S9(9)V COMP-3.
           10  93RT-CARGMT-RELAT-DESC          PIC S9(3)V COMP-3.
           10  93RT-NSEQ-ARGMT-SOLCT           PIC S9(3)V COMP-3.
           10  93RT-RARGMT-SOLTC-RELAT         PIC X(30).
           10  93RT-HULT-ATULZ                 PIC X(26).
           10  93RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  93RT-CTERM                      PIC X(8).
