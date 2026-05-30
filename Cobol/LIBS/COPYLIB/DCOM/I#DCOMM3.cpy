      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0M3 - DB2PRD.TCONTR_DESC_COML                        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7595 - SELECT         - DCOM7596 - FETCH             *
      *    - DCOM7597 - INSERT         - DCOM7598 - UPDATE            *
      *    - DCOM7599 - DELETE                                        *
      *===============================================================*

       01  M3-ARGUMENTOS-ENTRADA.

       05  M3-COLUNAS-DA-TABELA.
           10 M3-CPSSOA-JURID-CONTR    PIC S9(010) COMP-3.
           10 M3-CTPO-CONTR-NEGOC      PIC S9(003) COMP-3.
           10 M3-NSEQ-CONTR-NEGOC      PIC S9(010) COMP-3.
           10 M3-DANO-OPER-DESC        PIC S9(004) COMP-3.
           10 M3-NSEQ-OPER-DESC        PIC S9(009) COMP-3.
           10 M3-CPRODT-SERVC-OPER     PIC S9(008) COMP-3.
           10 M3-HULT-ATULZ            PIC  X(026).
           10 M3-CFUNC-BDSCO           PIC S9(009) COMP-3.
           10 M3-CTERM                 PIC  X(008).
           10 M3-CPPSTA-PRODT          PIC S9(019) COMP-3.
           10 M3-NSEQ-CONTR-LIM        PIC S9(017) COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  M3-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  M3-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  M3-RETORNO.
       05  M3-COD-RETORNO              PIC  9(004).
       05  M3-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  M3-PROXIMO-RESTART          PIC  9(005).
       05  M3-QTDE-RETORNADA           PIC  9(003).
       05  M3-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  M3-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TM3.
           10 M3RT-CPSSOA-JURID-CONTR  PIC S9(010) COMP-3.
           10 M3RT-CTPO-CONTR-NEGOC    PIC S9(003) COMP-3.
           10 M3RT-NSEQ-CONTR-NEGOC    PIC S9(010) COMP-3.
           10 M3RT-DANO-OPER-DESC      PIC S9(004) COMP-3.
           10 M3RT-NSEQ-OPER-DESC      PIC S9(009) COMP-3.
           10 M3RT-CPRODT-SERVC-OPER   PIC S9(008) COMP-3.
           10 M3RT-HULT-ATULZ          PIC  X(026).
           10 M3RT-CFUNC-BDSCO         PIC S9(009) COMP-3.
           10 M3RT-CTERM               PIC  X(008).
           10 M3RT-CPPSTA-PRODT        PIC S9(019) COMP-3.
           10 M3RT-NSEQ-CONTR-LIM      PIC S9(017) COMP-3.

