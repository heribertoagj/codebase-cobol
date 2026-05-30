      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H6 - DB2PRD.TSMULA_CTA_CHEQC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  H6-ARGUMENTOS-ENTRADA.

       05  H6-COLUNAS-DA-TABELA.
           10  H6-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  H6-CBCO-CLI-CHEQC               PIC S9(3)V COMP-3.
           10  H6-CDEPDC                       PIC S9(5)V COMP-3.
           10  H6-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  H6-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  H6-CINDCD-SELEC-DESC            PIC X(1).
           10  H6-HULT-ATULZ                   PIC X(26).
           10  H6-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H6-CTERM                        PIC X(8).
           10  H6-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H6-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H6-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H6-RETORNO.
       05  H6-COD-RETORNO                         PIC 9(04).
       05  H6-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H6-PROXIMO-RESTART                     PIC 9(05).
       05  H6-QTDE-RETORNADA                      PIC 9(03).
       05  H6-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH6.
           10  H6RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  H6RT-CBCO-CLI-CHEQC             PIC S9(3)V COMP-3.
           10  H6RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  H6RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  H6RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  H6RT-CINDCD-SELEC-DESC          PIC X(1).
           10  H6RT-HULT-ATULZ                 PIC X(26).
           10  H6RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H6RT-CTERM                      PIC X(8).
           10  H6RT-DINCL-REG                  PIC X(10).
