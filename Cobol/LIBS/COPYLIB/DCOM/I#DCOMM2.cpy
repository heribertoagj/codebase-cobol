      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0M2 - DB2PRD.TSPROD_PRZ_RURAL                        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7590 - SELECT         - DCOM7591 - FETCH             *
      *    - DCOM7592 - INSERT         - DCOM7593 - UPDATE            *
      *    - DCOM7594 - DELETE                                        *
      *===============================================================*

       01  M2-ARGUMENTOS-ENTRADA.

       05  M2-COLUNAS-DA-TABELA.
           10  M2-CPRODT               PIC S9(003) COMP-3.
           10  M2-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  M2-CPRODT-RURAL         PIC S9(003) COMP-3.
           10  M2-CVAR-PRODT-RURAL     PIC S9(005) COMP-3.
           10  M2-QDIA-PRZ-DESC        PIC S9(003) COMP-3.
           10  M2-HULT-ATULZ           PIC  X(026).
           10  M2-CFUNC-BDSCO          PIC S9(009) COMP-3.
           10  M2-CTERM                PIC  X(008).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  M2-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  M2-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  M2-RETORNO.
       05  M2-COD-RETORNO              PIC  9(004).
       05  M2-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  M2-PROXIMO-RESTART          PIC  9(005).
       05  M2-QTDE-RETORNADA           PIC  9(003).
       05  M2-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  M2-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TM2.
           10  M2RT-CPRODT             PIC S9(003) COMP-3.
           10  M2RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  M2RT-CPRODT-RURAL       PIC S9(003) COMP-3.
           10  M2RT-CVAR-PRODT-RURAL   PIC S9(005) COMP-3.
           10  M2RT-QDIA-PRZ-DESC      PIC S9(003) COMP-3.
           10  M2RT-HULT-ATULZ         PIC  X(026).
           10  M2RT-CFUNC-BDSCO        PIC S9(009) COMP-3.
           10  M2RT-CTERM              PIC  X(008).

