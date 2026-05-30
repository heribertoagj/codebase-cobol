      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0D2 - DB2PRD.TPROVS_INADP_DEPDC                     *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7160 - SELECT         - DCOM7161 - FETCH             *
      *    - DCOM7162 - INSERT         - DCOM7163 - UPDATE            *
      *    - DCOM7164 - DELETE                                        *
      *===============================================================*

       01  D2-ARGUMENTOS-ENTRADA.

       05  D2-COLUNAS-DA-TABELA.
           10  D2-DPOSIC-CONSL-DESC    PIC  X(010).
           10  D2-CINDCD-TPO-CONSL     PIC S9(001) COMP-3.
           10  D2-CEMPR-INC            PIC S9(005) COMP-3.
           10  D2-CDEPDC               PIC S9(005) COMP-3.
           10  D2-CGERC-RGNAL          PIC S9(005) COMP-3.
           10  D2-CAG-BCRIA            PIC S9(005) COMP-3.
           10  D2-CPRODT               PIC S9(003) COMP-3.
           10  D2-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  D2-CFAIXA-PRZ-CONSL     PIC S9(003) COMP-3.
           10  D2-CINDCD-PER-CONSL     PIC  X(001).
           10  D2-CORIGE-OPER-DESC     PIC S9(001)      COMP-3.
           10  D2-VPRINC-PCELA-CONSL   PIC S9(015)V9(2) COMP-3.
           10  D2-VENCAR-PCELA-CONSL   PIC S9(015)V9(2) COMP-3.
           10  D2-VTOT-POSIC-CONSL     PIC S9(015)V9(2) COMP-3.
           10  D2-QTITLO-POSIC-CONSL   PIC S9(015)      COMP-3.
           10  D2-PFAIXA-POSIC-CONSL   PIC S9(003)V9(2) COMP-3.
           10  D2-VPSENT-PCELA-DESC    PIC S9(015)V9(2) COMP-3.
           10  D2-VRAP-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D2-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D2-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D2-RETORNO.
       05  D2-COD-RETORNO              PIC  9(004).
       05  D2-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D2-PROXIMO-RESTART          PIC  9(005).
       05  D2-QTDE-RETORNADA           PIC  9(003).
       05  D2-QTDE-TOTAL               PIC  9(009).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D2-OCORRENCIAS       OCCURS 070 TIMES INDEXED BY IND-TD2.
           10  D2RT-DPOSIC-CONSL-DESC  PIC  X(010).
           10  D2RT-CINDCD-TPO-CONSL   PIC S9(001) COMP-3.
           10  D2RT-CEMPR-INC          PIC S9(005) COMP-3.
           10  D2RT-CDEPDC             PIC S9(005) COMP-3.
           10  D2RT-CGERC-RGNAL        PIC S9(005) COMP-3.
           10  D2RT-CAG-BCRIA          PIC S9(005) COMP-3.
           10  D2RT-CPRODT             PIC S9(003) COMP-3.
           10  D2RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  D2RT-CFAIXA-PRZ-CONSL   PIC S9(003) COMP-3.
           10  D2RT-CINDCD-PER-CONSL   PIC  X(001).
           10  D2RT-CORIGE-OPER-DESC   PIC S9(001)      COMP-3.
           10  D2RT-VPRINC-PCELA-CONSL PIC S9(015)V9(2) COMP-3.
           10  D2RT-VENCAR-PCELA-CONSL PIC S9(015)V9(2) COMP-3.
           10  D2RT-VTOT-POSIC-CONSL   PIC S9(015)V9(2) COMP-3.
           10  D2RT-QTITLO-POSIC-CONSL PIC S9(015)      COMP-3.
           10  D2RT-PFAIXA-POSIC-CONSL PIC S9(003)V9(2) COMP-3.
           10  D2RT-VPSENT-PCELA-DESC  PIC S9(015)V9(2) COMP-3.
           10  D2RT-VRAP-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
