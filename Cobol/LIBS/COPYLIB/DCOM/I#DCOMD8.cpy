      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0D8 - DB2PRD.TSMULA_BDERO_CTDIA     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  D8-ARGUMENTOS-ENTRADA.

       05  D8-COLUNAS-DA-TABELA.
           10  D8-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  D8-CBCO-CLI-CHEQC               PIC S9(3)V COMP-3.
           10  D8-CDEPDC                       PIC S9(5)V COMP-3.
           10  D8-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  D8-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  D8-CPROT-LOTE-CHEQC             PIC S9(7)V COMP-3.
           10  D8-DMOVTO                       PIC X(10).
           10  D8-QCHEQ-BDERO                  PIC S9(9)V COMP-3.
           10  D8-VTOT-CHEQC-BDERO             PIC S9(13)V9(2) COMP-3.
           10  D8-CSIT-BDERO                   PIC S9(1)V COMP-3.
           10  D8-CINDCD-SELEC-DESC            PIC X(1).
           10  D8-HULT-ATULZ                   PIC X(26).
           10  D8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  D8-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D8-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D8-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D8-RETORNO.
       05  D8-COD-RETORNO                         PIC 9(04).
       05  D8-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D8-PROXIMO-RESTART                     PIC 9(05).
       05  D8-QTDE-RETORNADA                      PIC 9(03).
       05  D8-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TD8.
           10  D8RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  D8RT-CBCO-CLI-CHEQC             PIC S9(3)V COMP-3.
           10  D8RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  D8RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  D8RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  D8RT-CPROT-LOTE-CHEQC           PIC S9(7)V COMP-3.
           10  D8RT-DMOVTO                     PIC X(10).
           10  D8RT-QCHEQ-BDERO                PIC S9(9)V COMP-3.
           10  D8RT-VTOT-CHEQC-BDERO           PIC S9(13)V9(2) COMP-3.
           10  D8RT-CSIT-BDERO                 PIC S9(1)V COMP-3.
           10  D8RT-CINDCD-SELEC-DESC          PIC X(1).
           10  D8RT-HULT-ATULZ                 PIC X(26).
           10  D8RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  D8RT-CTERM                      PIC X(8).
