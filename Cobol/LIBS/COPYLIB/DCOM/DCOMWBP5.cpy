      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0P5 - DB2PRD.TPROT_SOLTC_AGNDA                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7720 - SELECT         - DCOM7721 - FETCH             *
      *    - DCOM7722 - INSERT         - DCOM7723 - UPDATE            *
      *    - DCOM7724 - DELETE                                        *
      *===============================================================*

       01  DCOMWBP5-ENTRADA.
           05 DCOMWBP5-COLUNAS-DA-TABELA.
              10 DCOMWBP5-CBCO               PIC S9(03)V COMP-3.
              10 DCOMWBP5-CAG-BCRIA          PIC S9(05)V COMP-3.
              10 DCOMWBP5-CCTA-BCRIA-CLI     PIC S9(13)V COMP-3.
              10 DCOMWBP5-CPROT-SOLTC-AGNDA  PIC  X(50).
              10 DCOMWBP5-CTPO-PROT-AGNDA    PIC  X(01).
              10 DCOMWBP5-QTEMPO-VALDD-PROCM PIC S9(03)V COMP-3.
              10 DCOMWBP5-CSIT-PROT-AGNDA    PIC S9(01)V COMP-3.
              10 DCOMWBP5-HINCL-REG          PIC  X(26).
              10 DCOMWBP5-NSMULA-OPER-DESC   PIC S9(09)V COMP-3.
              10 DCOMWBP5-DANO-OPER-DESC     PIC S9(04)V COMP-3.
              10 DCOMWBP5-NSEQ-OPER-DESC     PIC S9(09)V COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
           05 DCOMWBP5-QTDE-A-DESPREZAR PIC 9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
           05 DCOMWBP5-INSTRUCAO       PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************

       01  DCOMWBP5-RETORNO.
           05 DCOMWBP5-COD-RETORNO     PIC  9(004).
           05 DCOMWBP5-MENSAGEM        PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
           05 DCOMWBP5-PROXIMO-RESTART PIC  9(005).
           05 DCOMWBP5-QTDE-RETORNADA  PIC  9(003).
           05 DCOMWBP5-QTDE-TOTAL      PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
           05 DCOMWBP5-OCORRENCIAS OCCURS 070 TIMES INDEXED BY IND-TP5.
              10 DCOMWBP5-RT-CBCO               PIC S9(03)V  COMP-3.
              10 DCOMWBP5-RT-CAG-BCRIA          PIC S9(05)V  COMP-3.
              10 DCOMWBP5-RT-CCTA-BCRIA-CLI     PIC S9(13)V  COMP-3.
              10 DCOMWBP5-RT-CPROT-SOLTC-AGNDA  PIC  X(50).
              10 DCOMWBP5-RT-CTPO-PROT-AGNDA    PIC  X(01).
              10 DCOMWBP5-RT-QTEMPO-VALDD-PROCM PIC S9(03)V  COMP-3.
              10 DCOMWBP5-RT-CSIT-PROT-AGNDA    PIC S9(01)V  COMP-3.
              10 DCOMWBP5-RT-HINCL-REG          PIC  X(26).
              10 DCOMWBP5-RT-NSMULA-OPER-DESC   PIC S9(09)V  COMP-3.
              10 DCOMWBP5-RT-DANO-OPER-DESC     PIC S9(04)V  COMP-3.
              10 DCOMWBP5-RT-NSEQ-OPER-DESC     PIC S9(09)V  COMP-3.
