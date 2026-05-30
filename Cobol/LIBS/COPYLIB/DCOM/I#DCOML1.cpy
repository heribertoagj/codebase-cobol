      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0L1 - DB2PRD.TTITLO_ANALD_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************

       01  L1-ARGUMENTOS-ENTRADA.

       05  L1-COLUNAS-DA-TABELA.
           10  L1-NSEQ-REG-ANLSE               PIC S9(12)V     COMP-3.
           10  L1-CPTCAO-TBELA                 PIC S9(3)V      COMP-3.
           10  L1-CBCO-PRODT-COBR              PIC S9(3)V      COMP-3.
           10  L1-CIDTFD-PRODT-COBR            PIC S9(2)V      COMP-3.
           10  L1-CNEGOC-COBR                  PIC S9(18)V     COMP-3.
           10  L1-CTITLO-COBR-BCO              PIC S9(18)V     COMP-3.
           10  L1-CNRO-SEQ-TITLO               PIC S9(3)V      COMP-3.
           10  L1-DVCTO-TITLO-DESC             PIC X(10).
           10  L1-VTITLO-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  L1-CINDCD-TITLO-ANALD           PIC X(1).
           10  L1-RANLSE-CREDT-CLI             PIC X(20).
           10  L1-DSOLTC-ANLSE-CREDT           PIC X(10).
           10  L1-CSIT-ANLSE-TITLO             PIC S9(3)V      COMP-3.
           10  L1-DINCL-REG                    PIC X(10).
           10  L1-HULT-ATULZ                   PIC X(26).
           10  L1-CFUNC-BDSCO                  PIC S9(9)V      COMP-3.
           10  L1-CTERM                        PIC X(8).

      * AREA AUXILIAR PARA ACESSOS ESPECIAIS
      *-------------------------------------*
       05  L1-VAGO                               PIC X(100).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  L1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  L1-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  L1-RETORNO.
       05  L1-COD-RETORNO                         PIC 9(04).
       05  L1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  L1-PROXIMO-RESTART                     PIC 9(05).
       05  L1-QTDE-RETORNADA                      PIC 9(03).
       05  L1-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  L1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TL1.
           10  L1RT-NSEQ-REG-ANLSE             PIC S9(12)V     COMP-3.
           10  L1RT-CPTCAO-TBELA               PIC 9(3)V       COMP-3.
           10  L1RT-CBCO-PRODT-COBR            PIC S9(3)V      COMP-3.
           10  L1RT-CIDTFD-PRODT-COBR          PIC S9(2)V      COMP-3.
           10  L1RT-CNEGOC-COBR                PIC S9(18)V     COMP-3.
           10  L1RT-CTITLO-COBR-BCO            PIC S9(18)V     COMP-3.
           10  L1RT-CNRO-SEQ-TITLO             PIC S9(3)V      COMP-3.
           10  L1RT-DVCTO-TITLO-DESC           PIC X(10).
           10  L1RT-VTITLO-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  L1RT-CINDCD-TITLO-ANALD         PIC X(1).
           10  L1RT-RANLSE-CREDT-CLI           PIC X(20).
           10  L1RT-DSOLTC-ANLSE-CREDT         PIC X(10).
           10  L1RT-CSIT-ANLSE-TITLO           PIC S9(3)V      COMP-3.
           10  L1RT-DINCL-REG                  PIC X(10).
           10  L1RT-HULT-ATULZ                 PIC X(26).
           10  L1RT-CFUNC-BDSCO                PIC S9(9)V      COMP-3.
           10  L1RT-CTERM                      PIC X(8).
