      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F8 - DB2PRD.TTRASI_CTBIL_CHEQC     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  F8-ARGUMENTOS-ENTRADA.

       05  F8-COLUNAS-DA-TABELA.
           10  F8-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  F8-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  F8-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  F8-CDEPDC                       PIC S9(5)V COMP-3.
           10  F8-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  F8-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  F8-VDESC-CLI-CTDIA              PIC S9(15)V9(2) COMP-3.
           10  F8-HULT-ATULZ                   PIC X(26).
           10  F8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  F8-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F8-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F8-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F8-RETORNO.
       05  F8-COD-RETORNO                         PIC 9(04).
       05  F8-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F8-PROXIMO-RESTART                     PIC 9(05).
       05  F8-QTDE-RETORNADA                      PIC 9(03).
       05  F8-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF8.
           10  F8RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  F8RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  F8RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  F8RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  F8RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  F8RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  F8RT-VDESC-CLI-CTDIA            PIC S9(15)V9(2) COMP-3.
           10  F8RT-HULT-ATULZ                 PIC X(26).
           10  F8RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  F8RT-CTERM                      PIC X(8).
