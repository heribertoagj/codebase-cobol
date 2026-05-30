      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0M1 - DB2PRD.TSPROD_BACEN_REGRA                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7585 - SELECT         - DCOM7586 - FETCH             *
      *    - DCOM7587 - INSERT         - DCOM7588 - UPDATE            *
      *    - DCOM7589 - DELETE                                        *
      *===============================================================*

       01  M1-ARGUMENTOS-ENTRADA.

       05  M1-COLUNAS-DA-TABELA.
           10  M1-CPRODT               PIC S9(003) COMP-3.
           10  M1-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  M1-HINIC-REGRA-OPER     PIC  X(026).
           10  M1-CTPO-REG-BACEN       PIC S9(001) COMP-3.
           10  M1-QDIA-ALT-BACEN       PIC S9(003) COMP-3.
           10  M1-CINDCD-DIA-BACEN     PIC  X(001).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  M1-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  M1-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  M1-RETORNO.
       05  M1-COD-RETORNO              PIC  9(004).
       05  M1-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  M1-PROXIMO-RESTART          PIC  9(005).
       05  M1-QTDE-RETORNADA           PIC  9(003).
       05  M1-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  M1-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TM1.
           10  M1RT-CPRODT             PIC S9(003) COMP-3.
           10  M1RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  M1RT-HINIC-REGRA-OPER   PIC  X(026).
           10  M1RT-CTPO-REG-BACEN     PIC S9(001) COMP-3.
           10  M1RT-QDIA-ALT-BACEN     PIC S9(003) COMP-3.
           10  M1RT-CINDCD-DIA-BACEN   PIC  X(001).

