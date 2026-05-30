      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F3 - DB2PRD.TTRASI_BDERO_CTDIA     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  F3-ARGUMENTOS-ENTRADA.

       05  F3-COLUNAS-DA-TABELA.
           10  F3-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  F3-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  F3-CBCO-CLI-CHEQC               PIC S9(3)V COMP-3.
           10  F3-CDEPDC                       PIC S9(5)V COMP-3.
           10  F3-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  F3-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  F3-CPROT-LOTE-CHEQC             PIC S9(7)V COMP-3.
           10  F3-DMOVTO                       PIC X(10).
           10  F3-QCHEQ-BDERO                  PIC S9(9)V COMP-3.
           10  F3-VTOT-CHEQC-BDERO             PIC S9(13)V9(2) COMP-3.
           10  F3-CSIT-BDERO                   PIC S9(1)V COMP-3.
           10  F3-CINDCD-SELEC-DESC            PIC X(1).
           10  F3-HULT-ATULZ                   PIC X(26).
           10  F3-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  F3-CTERM                        PIC X(8).
           10  F3-DINCL-REG                    PIC X(10).
PRM02      10  F3-CSIT-PIOR-CHEQ               PIC S9(1)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F3-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F3-RETORNO.
       05  F3-COD-RETORNO                         PIC 9(04).
       05  F3-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F3-PROXIMO-RESTART                     PIC 9(05).
       05  F3-QTDE-RETORNADA                      PIC 9(03).
       05  F3-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF3.
           10  F3RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  F3RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  F3RT-CBCO-CLI-CHEQC             PIC S9(3)V COMP-3.
           10  F3RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  F3RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  F3RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  F3RT-CPROT-LOTE-CHEQC           PIC S9(7)V COMP-3.
           10  F3RT-DMOVTO                     PIC X(10).
           10  F3RT-QCHEQ-BDERO                PIC S9(9)V COMP-3.
           10  F3RT-VTOT-CHEQC-BDERO           PIC S9(13)V9(2) COMP-3.
           10  F3RT-CSIT-BDERO                 PIC S9(1)V COMP-3.
           10  F3RT-CINDCD-SELEC-DESC          PIC X(1).
           10  F3RT-HULT-ATULZ                 PIC X(26).
           10  F3RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  F3RT-CTERM                      PIC X(8).
           10  F3RT-DINCL-REG                  PIC X(10).
PRM02      10  F3RT-CSIT-PIOR-CHEQ             PIC S9(1)V COMP-3.

