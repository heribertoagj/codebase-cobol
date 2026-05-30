      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K6 - DB2PRD.TDESPR_CHEQC_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K6-ARGUMENTOS-ENTRADA.

       05  K6-COLUNAS-DA-TABELA.
           10  K6-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  K6-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  K6-CBCO                         PIC S9(3)V COMP-3.
           10  K6-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  K6-CCTA-CORR-BCRIA              PIC S9(11)V COMP-3.
           10  K6-CCHEQ                        PIC S9(7)V COMP-3.
           10  K6-CBCO-CLI-CHEQC               PIC S9(3)V COMP-3.
           10  K6-CDEPDC                       PIC S9(5)V COMP-3.
           10  K6-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  K6-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  K6-CPROT-LOTE-CHEQC             PIC S9(7)V COMP-3.
           10  K6-DMOVTO                       PIC X(10).
           10  K6-CLOTE-CHEQC                  PIC S9(7)V COMP-3.
           10  K6-DVCTO-CHEQ-CTDIA             PIC X(10).
           10  K6-VCHEQC                       PIC S9(13)V9(2) COMP-3.
           10  K6-CINDCD-CHEQ-SELEC            PIC X(1).
           10  K6-CTPO-LOTE-OPER               PIC X(1).
           10  K6-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  K6-RANLSE-CREDT-CLI             PIC X(20).
           10  K6-DSOLTC-ANLSE-CREDT           PIC X(10).
           10  K6-CSIT-ANLSE-CREDT             PIC S9(3)V COMP-3.
           10  K6-CINDCD-ISENC-ANLSE           PIC X(1).
           10  K6-HULT-ATULZ                   PIC X(26).
           10  K6-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  K6-CTERM                        PIC X(8).
           10  K6-DINCL-REG                    PIC X(10).
           10  K6-CCNPJ-CPF-EMITT              PIC S9(9)V COMP-3.
           10  K6-CFLIAL-CNPJ-EMITT            PIC S9(5)V COMP-3.
           10  K6-CCTRL-CNPJ-EMITT             PIC S9(2)V COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K6-QTDE-A-DESPREZAR                   PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  K6-INSTRUCAO                          PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K6-RETORNO.
       05  K6-COD-RETORNO                         PIC 9(04).
       05  K6-MENSAGEM                            PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K6-PROXIMO-RESTART                     PIC 9(05).
       05  K6-QTDE-RETORNADA                      PIC 9(03).
       05  K6-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK6.
           10  K6RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  K6RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  K6RT-CBCO                       PIC S9(3)V COMP-3.
           10  K6RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  K6RT-CCTA-CORR-BCRIA            PIC S9(11)V COMP-3.
           10  K6RT-CCHEQ                      PIC S9(7)V COMP-3.
           10  K6RT-CBCO-CLI-CHEQC             PIC S9(3)V COMP-3.
           10  K6RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  K6RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  K6RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  K6RT-CPROT-LOTE-CHEQC           PIC S9(7)V COMP-3.
           10  K6RT-DMOVTO                     PIC X(10).
           10  K6RT-CLOTE-CHEQC                PIC S9(7)V COMP-3.
           10  K6RT-DVCTO-CHEQ-CTDIA           PIC X(10).
           10  K6RT-VCHEQC                     PIC S9(13)V9(2) COMP-3.
           10  K6RT-CINDCD-CHEQ-SELEC          PIC X(1).
           10  K6RT-CTPO-LOTE-OPER             PIC X(1).
           10  K6RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  K6RT-RANLSE-CREDT-CLI           PIC X(20).
           10  K6RT-DSOLTC-ANLSE-CREDT         PIC X(10).
           10  K6RT-CSIT-ANLSE-CREDT           PIC S9(3)V COMP-3.
           10  K6RT-CINDCD-ISENC-ANLSE         PIC X(1).
           10  K6RT-HULT-ATULZ                 PIC X(26).
           10  K6RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  K6RT-CTERM                      PIC X(8).
           10  K6RT-DINCL-REG                  PIC X(10).
           10  K6RT-CCNPJ-CPF-EMITT            PIC S9(9)V COMP-3.
           10  K6RT-CFLIAL-CNPJ-EMITT          PIC S9(5)V COMP-3.
           10  K6RT-CCTRL-CNPJ-EMITT           PIC S9(2)V COMP-3.

