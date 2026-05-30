      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G3 - DB2PRD.TTRASI_LOTE_CTDIA      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  G3-ARGUMENTOS-ENTRADA.

       05  G3-COLUNAS-DA-TABELA.
           10  G3-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  G3-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  G3-CBCO-CLI-CHEQC               PIC S9(3)V COMP-3.
           10  G3-CDEPDC                       PIC S9(5)V COMP-3.
           10  G3-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  G3-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  G3-CPROT-LOTE-CHEQC             PIC S9(7)V COMP-3.
           10  G3-DMOVTO                       PIC X(10).
           10  G3-CLOTE-CHEQC                  PIC S9(7)V COMP-3.
           10  G3-QCHEQ-LOTE                   PIC S9(9)V COMP-3.
           10  G3-VTOT-CHEQC-LOTE              PIC S9(15)V9(2) COMP-3.
           10  G3-CINDCD-SELEC-DESC            PIC X(1).
           10  G3-HULT-ATULZ                   PIC X(26).
           10  G3-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G3-CTERM                        PIC X(8).
           10  G3-DINCL-REG                    PIC X(10).
PRM02      10  G3-CSIT-PIOR-CHEQ               PIC S9(1)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G3-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G3-RETORNO.
       05  G3-COD-RETORNO                         PIC 9(04).
       05  G3-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G3-PROXIMO-RESTART                     PIC 9(05).
       05  G3-QTDE-RETORNADA                      PIC 9(03).
       05  G3-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG3.
           10  G3RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  G3RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  G3RT-CBCO-CLI-CHEQC             PIC S9(3)V COMP-3.
           10  G3RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  G3RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  G3RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  G3RT-CPROT-LOTE-CHEQC           PIC S9(7)V COMP-3.
           10  G3RT-DMOVTO                     PIC X(10).
           10  G3RT-CLOTE-CHEQC                PIC S9(7)V COMP-3.
           10  G3RT-QCHEQ-LOTE                 PIC S9(9)V COMP-3.
           10  G3RT-VTOT-CHEQC-LOTE            PIC S9(15)V9(2) COMP-3.
           10  G3RT-CINDCD-SELEC-DESC          PIC X(1).
           10  G3RT-HULT-ATULZ                 PIC X(26).
           10  G3RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G3RT-CTERM                      PIC X(8).
           10  G3RT-DINCL-REG                  PIC X(10).
PRM02      10  G3RT-CSIT-PIOR-CHEQ             PIC S9(1)V COMP-3.

