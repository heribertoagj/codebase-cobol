      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B0 - DB2PRD.THIST_CTBIL_CHEQC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  B0-ARGUMENTOS-ENTRADA.

       05  B0-COLUNAS-DA-TABELA.
           10  B0-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  B0-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  B0-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  B0-CDEPDC                       PIC S9(5)V COMP-3.
           10  B0-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  B0-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  B0-HULT-ATULZ                   PIC X(26).
           10  B0-VDESC-CLI-CTDIA              PIC S9(15)V9(2) COMP-3.
           10  B0-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  B0-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B0-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B0-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B0-RETORNO.
       05  B0-COD-RETORNO                         PIC 9(04).
       05  B0-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B0-PROXIMO-RESTART                     PIC 9(05).
       05  B0-QTDE-RETORNADA                      PIC 9(03).
       05  B0-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB0.
           10  B0RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  B0RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  B0RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  B0RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  B0RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  B0RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  B0RT-HULT-ATULZ                 PIC X(26).
           10  B0RT-VDESC-CLI-CTDIA            PIC S9(15)V9(2) COMP-3.
           10  B0RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  B0RT-CTERM                      PIC X(8).
