      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H0 - DB2PRD.TCONTR_CONS_CREDT      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  H0-ARGUMENTOS-ENTRADA.

       05  H0-COLUNAS-DA-TABELA.
           10  H0-CCONTR-LIM-DESC              PIC S9(9)V COMP-3.
           10  H0-CVRSAO-CONTR-LIM             PIC S9(3)V COMP-3.
           10  H0-NSEQ-CONS-CREDT              PIC S9(3)V COMP-3.
           10  H0-DANO-BASE                    PIC S9(4)V COMP-3.
           10  H0-CDEPDC                       PIC S9(5)V COMP-3.
           10  H0-CSEQ-STUDO                   PIC S9(9)V COMP-3.
           10  H0-VCONS-NEGOC                  PIC S9(15)V9(2) COMP-3.
           10  H0-CCART-LIM-CREDT              PIC X(5).
           10  H0-HULT-ATULZ                   PIC X(26).
           10  H0-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H0-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H0-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H0-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H0-RETORNO.
       05  H0-COD-RETORNO                         PIC 9(04).
       05  H0-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H0-PROXIMO-RESTART                     PIC 9(05).
       05  H0-QTDE-RETORNADA                      PIC 9(03).
       05  H0-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH0.
           10  H0RT-CCONTR-LIM-DESC            PIC S9(9)V COMP-3.
           10  H0RT-CVRSAO-CONTR-LIM           PIC S9(3)V COMP-3.
           10  H0RT-NSEQ-CONS-CREDT            PIC S9(3)V COMP-3.
           10  H0RT-DANO-BASE                  PIC S9(4)V COMP-3.
           10  H0RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  H0RT-CSEQ-STUDO                 PIC S9(9)V COMP-3.
           10  H0RT-VCONS-NEGOC                PIC S9(15)V9(2) COMP-3.
           10  H0RT-CCART-LIM-CREDT            PIC X(5).
           10  H0RT-HULT-ATULZ                 PIC X(26).
           10  H0RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H0RT-CTERM                      PIC X(8).
