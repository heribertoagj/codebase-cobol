      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0E4 - DB2PRD.TSMULA_LOTE_CTDIA      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  E4-ARGUMENTOS-ENTRADA.

       05  E4-COLUNAS-DA-TABELA.
           10  E4-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  E4-CBCO-CLI-CHEQC               PIC S9(3)V COMP-3.
           10  E4-CDEPDC                       PIC S9(5)V COMP-3.
           10  E4-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  E4-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  E4-CPROT-LOTE-CHEQC             PIC S9(7)V COMP-3.
           10  E4-DMOVTO                       PIC X(10).
           10  E4-CLOTE-CHEQC                  PIC S9(7)V COMP-3.
           10  E4-QCHEQ-LOTE                   PIC S9(9)V COMP-3.
           10  E4-VTOT-CHEQC-LOTE              PIC S9(15)V9(2) COMP-3.
           10  E4-CINDCD-SELEC-DESC            PIC X(1).
           10  E4-HULT-ATULZ                   PIC X(26).
           10  E4-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  E4-CTERM                        PIC X(8).
           10  E4-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E4-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E4-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E4-RETORNO.
       05  E4-COD-RETORNO                         PIC 9(04).
       05  E4-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E4-PROXIMO-RESTART                     PIC 9(05).
       05  E4-QTDE-RETORNADA                      PIC 9(03).
       05  E4-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE4.
           10  E4RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  E4RT-CBCO-CLI-CHEQC             PIC S9(3)V COMP-3.
           10  E4RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  E4RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  E4RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  E4RT-CPROT-LOTE-CHEQC           PIC S9(7)V COMP-3.
           10  E4RT-DMOVTO                     PIC X(10).
           10  E4RT-CLOTE-CHEQC                PIC S9(7)V COMP-3.
           10  E4RT-QCHEQ-LOTE                 PIC S9(9)V COMP-3.
           10  E4RT-VTOT-CHEQC-LOTE            PIC S9(15)V9(2) COMP-3.
           10  E4RT-CINDCD-SELEC-DESC          PIC X(1).
           10  E4RT-HULT-ATULZ                 PIC X(26).
           10  E4RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E4RT-CTERM                      PIC X(8).
           10  E4RT-DINCL-REG                  PIC X(10).
