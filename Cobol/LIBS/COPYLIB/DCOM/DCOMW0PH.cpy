      *================================================================*
      * INCLUDE PARA TABELA: DCOMB0PH - DB2PRD.TTRASI_CONTR_DESC_COML  *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM77XX - SELECT         - DCOM77XX - FETCH              *
      *    - DCOM77XX - INSERT         - DCOM77XX - UPDATE             *
      *    - DCOM77XX - DELETE                                         *
      *================================================================*

       01  DCOMW0PH-ARGUMENTOS-ENTRADA.

       05  DCOMW0PH-COLUNAS-DA-TABELA.
           10 DCOMW0PH-CPSSOA-JURID-CONTR PIC S9(010) COMP-3.
           10 DCOMW0PH-CTPO-CONTR-NEGOC   PIC S9(003) COMP-3.
           10 DCOMW0PH-NSEQ-CONTR-NEGOC   PIC S9(010) COMP-3.
           10 DCOMW0PH-DANO-OPER-DESC     PIC S9(004) COMP-3.
           10 DCOMW0PH-NSEQ-OPER-DESC     PIC S9(009) COMP-3.
           10 DCOMW0PH-CPRODT-SERVC-OPER  PIC S9(008) COMP-3.
           10 DCOMW0PH-HULT-ATULZ         PIC  X(026).
           10 DCOMW0PH-CFUNC-BDSCO        PIC S9(009) COMP-3.
           10 DCOMW0PH-CTERM              PIC  X(008).
           10 DCOMW0PH-CPPSTA-PRODT       PIC S9(019) COMP-3.
           10 DCOMW0PH-NSEQ-CONTR-LIM     PIC S9(017) COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  DCOMW0PH-QTDE-A-DESPREZAR   PIC  9(005).
       05  DCOMW0PH-START-COUNT        PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  DCOMW0PH-INSTRUCAO          PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  DCOMW0PH-RETORNO.
       05  DCOMW0PH-COD-RETORNO        PIC  9(004).
       05  DCOMW0PH-MENSAGEM           PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  DCOMW0PH-PROXIMO-RESTART    PIC  9(005).
       05  DCOMW0PH-QTDE-RETORNADA     PIC  9(003).
       05  DCOMW0PH-QTDE-TOTAL         PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  DCOMW0PH-OCORRENCIAS OCCURS 070 TIMES INDEXED BY IND-TPH.
           10  DCOMW0PH-RT-CPSSOA-JURID-CONTR PIC S9(010) COMP-3.
           10  DCOMW0PH-RT-CTPO-CONTR-NEGOC   PIC S9(003) COMP-3.
           10  DCOMW0PH-RT-NSEQ-CONTR-NEGOC   PIC S9(010) COMP-3.
           10  DCOMW0PH-RT-DANO-OPER-DESC     PIC S9(004) COMP-3.
           10  DCOMW0PH-RT-NSEQ-OPER-DESC     PIC S9(009) COMP-3.
           10  DCOMW0PH-RT-CPRODT-SERVC-OPER  PIC S9(008) COMP-3.
           10  DCOMW0PH-RT-HULT-ATULZ         PIC  X(026).
           10  DCOMW0PH-RT-CFUNC-BDSCO        PIC S9(009) COMP-3.
           10  DCOMW0PH-RT-CTERM              PIC  X(008).
           10  DCOMW0PH-RT-CPPSTA-PRODT       PIC S9(019) COMP-3.
           10  DCOMW0PH-RT-NSEQ-CONTR-LIM     PIC S9(017) COMP-3.

