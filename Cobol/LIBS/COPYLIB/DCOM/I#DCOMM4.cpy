      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0M4 - DB2PRD.THIST_CONTR_DESC                        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7600 - SELECT         - DCOM7601 - FETCH             *
      *    - DCOM7602 - INSERT         - DCOM7603 - UPDATE            *
      *    - DCOM7604 - DELETE                                        *
      *===============================================================*

       01  M4-ARGUMENTOS-ENTRADA.

       05  M4-COLUNAS-DA-TABELA.
           10 M4-CPSSOA-JURID-CONTR    PIC S9(010) COMP-3.
           10 M4-CTPO-CONTR-NEGOC      PIC S9(003) COMP-3.
           10 M4-NSEQ-CONTR-NEGOC      PIC S9(010) COMP-3.
           10 M4-HINCL-REG-HIST        PIC  X(026).
           10 M4-DANO-OPER-DESC        PIC S9(004) COMP-3.
           10 M4-NSEQ-OPER-DESC        PIC S9(009) COMP-3.
           10 M4-CPRODT-SERVC-OPER     PIC S9(008) COMP-3.
           10 M4-HULT-ATULZ            PIC  X(026).
           10 M4-CFUNC-BDSCO           PIC S9(009) COMP-3.
           10 M4-CTERM                 PIC  X(008).
           10 M4-CPPSTA-PRODT          PIC S9(019) COMP-3.
           10 M4-NSEQ-CONTR-LIM        PIC S9(017) COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  M4-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  M4-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  M4-RETORNO.
       05  M4-COD-RETORNO              PIC  9(004).
       05  M4-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  M4-PROXIMO-RESTART          PIC  9(005).
       05  M4-QTDE-RETORNADA           PIC  9(003).
       05  M4-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  M4-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TM4.
           10 M4RT-CPSSOA-JURID-CONTR  PIC S9(010) COMP-3.
           10 M4RT-CTPO-CONTR-NEGOC    PIC S9(003) COMP-3.
           10 M4RT-NSEQ-CONTR-NEGOC    PIC S9(010) COMP-3.
           10 M4RT-HINCL-REG-HIST      PIC  X(026).
           10 M4RT-DANO-OPER-DESC      PIC S9(004) COMP-3.
           10 M4RT-NSEQ-OPER-DESC      PIC S9(009) COMP-3.
           10 M4RT-CPRODT-SERVC-OPER   PIC S9(008) COMP-3.
           10 M4RT-HULT-ATULZ          PIC  X(026).
           10 M4RT-CFUNC-BDSCO         PIC S9(009) COMP-3.
           10 M4RT-CTERM               PIC  X(008).
           10 M4RT-CPPSTA-PRODT        PIC S9(019) COMP-3.
           10 M4RT-NSEQ-CONTR-LIM      PIC S9(017) COMP-3.

