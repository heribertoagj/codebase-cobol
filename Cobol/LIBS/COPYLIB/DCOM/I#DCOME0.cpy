      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0E0 - DB2PRD.TSMULA_CHEQC_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  E0-ARGUMENTOS-ENTRADA.

       05  E0-COLUNAS-DA-TABELA.
           10  E0-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  E0-CBCO                         PIC S9(3)V COMP-3.
           10  E0-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  E0-CCTA-CORR-BCRIA              PIC S9(11)V COMP-3.
           10  E0-CCHEQ                        PIC S9(7)V COMP-3.
           10  E0-CBCO-CLI-CHEQC               PIC S9(3)V COMP-3.
           10  E0-CDEPDC                       PIC S9(5)V COMP-3.
           10  E0-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  E0-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  E0-CPROT-LOTE-CHEQC             PIC S9(7)V COMP-3.
           10  E0-DMOVTO                       PIC X(10).
           10  E0-CLOTE-CHEQC                  PIC S9(7)V COMP-3.
           10  E0-DVCTO-CHEQ-CTDIA             PIC X(10).
           10  E0-VCHEQC                       PIC S9(13)V9(2) COMP-3.
           10  E0-CINDCD-CHEQ-SELEC            PIC X(1).
           10  E0-CTPO-LOTE-OPER               PIC X(1).
           10  E0-HULT-ATULZ                   PIC X(26).
           10  E0-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  E0-CTERM                        PIC X(8).
           10  E0-DINCL-REG                    PIC X(10).
           10  E0-CCNPJ-CPF-EMITT              PIC S9(9)V COMP-3.
           10  E0-CFLIAL-CNPJ-EMITT            PIC S9(5)V COMP-3.
           10  E0-CCTRL-CNPJ-EMITT             PIC S9(2)V COMP-3.
DTS001     10  E0-RANLSE-CREDT-CLI             PIC X(20).
DTS001     10  E0-DSOLTC-ANLSE-CREDT           PIC X(10).
DTS001     10  E0-CSIT-ANLSE-CREDT             PIC S9(3)V COMP-3.
DTS001     10  E0-CINDCD-ISENC-ANLSE           PIC X(1).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E0-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E0-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E0-RETORNO.
       05  E0-COD-RETORNO                         PIC 9(04).
       05  E0-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E0-PROXIMO-RESTART                     PIC 9(05).
       05  E0-QTDE-RETORNADA                      PIC 9(03).
       05  E0-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE0.
           10  E0RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  E0RT-CBCO                       PIC S9(3)V COMP-3.
           10  E0RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  E0RT-CCTA-CORR-BCRIA            PIC S9(11)V COMP-3.
           10  E0RT-CCHEQ                      PIC S9(7)V COMP-3.
           10  E0RT-CBCO-CLI-CHEQC             PIC S9(3)V COMP-3.
           10  E0RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  E0RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  E0RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  E0RT-CPROT-LOTE-CHEQC           PIC S9(7)V COMP-3.
           10  E0RT-DMOVTO                     PIC X(10).
           10  E0RT-CLOTE-CHEQC                PIC S9(7)V COMP-3.
           10  E0RT-DVCTO-CHEQ-CTDIA           PIC X(10).
           10  E0RT-VCHEQC                     PIC S9(13)V9(2) COMP-3.
           10  E0RT-CINDCD-CHEQ-SELEC          PIC X(1).
           10  E0RT-CTPO-LOTE-OPER             PIC X(1).
           10  E0RT-HULT-ATULZ                 PIC X(26).
           10  E0RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E0RT-CTERM                      PIC X(8).
           10  E0RT-DINCL-REG                  PIC X(10).
           10  E0RT-CCNPJ-CPF-EMITT            PIC S9(9)V COMP-3.
           10  E0RT-CFLIAL-CNPJ-EMITT          PIC S9(5)V COMP-3.
           10  E0RT-CCTRL-CNPJ-EMITT           PIC S9(2)V COMP-3.
DTS001     10  E0RT-RANLSE-CREDT-CLI           PIC X(20).
DTS001     10  E0RT-DSOLTC-ANLSE-CREDT         PIC X(10).
DTS001     10  E0RT-CSIT-ANLSE-CREDT           PIC S9(3)V COMP-3.
DTS001     10  E0RT-CINDCD-ISENC-ANLSE         PIC X(1).
