      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F5 - DB2PRD.TTRASI_CHEQC_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  F5-ARGUMENTOS-ENTRADA.

       05  F5-COLUNAS-DA-TABELA.
           10  F5-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  F5-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  F5-CBCO                         PIC S9(3)V COMP-3.
           10  F5-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  F5-CCTA-CORR-BCRIA              PIC S9(11)V COMP-3.
           10  F5-CCHEQ                        PIC S9(7)V COMP-3.
           10  F5-CBCO-CLI-CHEQC               PIC S9(3)V COMP-3.
           10  F5-CDEPDC                       PIC S9(5)V COMP-3.
           10  F5-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  F5-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  F5-CPROT-LOTE-CHEQC             PIC S9(7)V COMP-3.
           10  F5-DMOVTO                       PIC X(10).
           10  F5-CLOTE-CHEQC                  PIC S9(7)V COMP-3.
           10  F5-DVCTO-CHEQ-CTDIA             PIC X(10).
           10  F5-VCHEQC                       PIC S9(13)V9(2) COMP-3.
           10  F5-CINDCD-CHEQ-SELEC            PIC X(1).
           10  F5-CTPO-LOTE-OPER               PIC X(1).
           10  F5-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  F5-RANLSE-CREDT-CLI             PIC X(20).
           10  F5-DSOLTC-ANLSE-CREDT           PIC X(10).
           10  F5-CSIT-ANLSE-CREDT             PIC S9(3)V COMP-3.
           10  F5-CINDCD-ISENC-ANLSE           PIC X(1).
           10  F5-HULT-ATULZ                   PIC X(26).
           10  F5-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  F5-CTERM                        PIC X(8).
           10  F5-DINCL-REG                    PIC X(10).
  LT       10  F5-CCNPJ-CPF-EMITT              PIC S9(9)V COMP-3.
  LT       10  F5-CFILIAL-CNPJ-EMITT           PIC S9(5)V COMP-3.
  LT       10  F5-CCTRL-CNPJ-EMITT             PIC S9(2)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F5-RETORNO.
       05  F5-COD-RETORNO                         PIC 9(04).
       05  F5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F5-PROXIMO-RESTART                     PIC 9(05).
       05  F5-QTDE-RETORNADA                      PIC 9(03).
       05  F5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF5.
           10  F5RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  F5RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  F5RT-CBCO                       PIC S9(3)V COMP-3.
           10  F5RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  F5RT-CCTA-CORR-BCRIA            PIC S9(11)V COMP-3.
           10  F5RT-CCHEQ                      PIC S9(7)V COMP-3.
           10  F5RT-CBCO-CLI-CHEQC             PIC S9(3)V COMP-3.
           10  F5RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  F5RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  F5RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  F5RT-CPROT-LOTE-CHEQC           PIC S9(7)V COMP-3.
           10  F5RT-DMOVTO                     PIC X(10).
           10  F5RT-CLOTE-CHEQC                PIC S9(7)V COMP-3.
           10  F5RT-DVCTO-CHEQ-CTDIA           PIC X(10).
           10  F5RT-VCHEQC                     PIC S9(13)V9(2) COMP-3.
           10  F5RT-CINDCD-CHEQ-SELEC          PIC X(1).
           10  F5RT-CTPO-LOTE-OPER             PIC X(1).
           10  F5RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  F5RT-RANLSE-CREDT-CLI           PIC X(20).
           10  F5RT-DSOLTC-ANLSE-CREDT         PIC X(10).
           10  F5RT-CSIT-ANLSE-CREDT           PIC S9(3)V COMP-3.
           10  F5RT-CINDCD-ISENC-ANLSE         PIC X(1).
           10  F5RT-HULT-ATULZ                 PIC X(26).
           10  F5RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  F5RT-CTERM                      PIC X(8).
           10  F5RT-DINCL-REG                  PIC X(10).
  LT       10  F5RT-CCNPJ-CPF-EMITT            PIC S9(9)V COMP-3.
  LT       10  F5RT-CFLIAL-CNPJ-EMITT          PIC S9(5)V COMP-3.
  LT       10  F5RT-CCTRL-CNPJ-EMITT           PIC S9(2)V COMP-3.
