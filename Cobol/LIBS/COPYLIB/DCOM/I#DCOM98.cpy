      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB098 - DB2PRD.TCTRL_CTBIL_CHEQC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  98-ARGUMENTOS-ENTRADA.

       05  98-COLUNAS-DA-TABELA.
           10  98-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  98-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  98-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  98-CDEPDC                       PIC S9(5)V COMP-3.
           10  98-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  98-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  98-VDESC-CLI-CTDIA              PIC S9(15)V9(2) COMP-3.
           10  98-HULT-ATULZ                   PIC X(26).
           10  98-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  98-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  98-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  98-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  98-RETORNO.
       05  98-COD-RETORNO                         PIC 9(04).
       05  98-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  98-PROXIMO-RESTART                     PIC 9(05).
       05  98-QTDE-RETORNADA                      PIC 9(03).
       05  98-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  98-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T98.
           10  98RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  98RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  98RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  98RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  98RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  98RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  98RT-VDESC-CLI-CTDIA            PIC S9(15)V9(2) COMP-3.
           10  98RT-HULT-ATULZ                 PIC X(26).
           10  98RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  98RT-CTERM                      PIC X(8).
