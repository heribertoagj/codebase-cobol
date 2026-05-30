      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0D1 - DB2PRD.TPROVS_INADP_CLI                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7155 - SELECT         - DCOM7156 - FETCH             *
      *    - DCOM7157 - INSERT         - DCOM7158 - UPDATE            *
      *    - DCOM7159 - DELETE                                        *
      *===============================================================*

       01  D1-ARGUMENTOS-ENTRADA.

       05  D1-COLUNAS-DA-TABELA.
           10  D1-DMES-CONSL-PTCAO     PIC S9(002) COMP-3.
           10  D1-DPOSIC-CONSL-DESC    PIC  X(010).
           10  D1-CINDCD-TPO-CONSL     PIC S9(001) COMP-3.
           10  D1-CBCO                 PIC S9(003) COMP-3.
           10  D1-CAG-BCRIA            PIC S9(005) COMP-3.
           10  D1-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  D1-CPRODT               PIC S9(003) COMP-3.
           10  D1-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  D1-CFAIXA-PRZ-CONSL     PIC S9(003) COMP-3.
           10  D1-CINDCD-PER-CONSL     PIC  X(001).
           10  D1-CORIGE-OPER-DESC     PIC S9(001)      COMP-3.
           10  D1-VPRINC-PCELA-CONSL   PIC S9(015)V9(2) COMP-3.
           10  D1-VENCAR-PCELA-CONSL   PIC S9(015)V9(2) COMP-3.
           10  D1-VTOT-POSIC-CONSL     PIC S9(015)V9(2) COMP-3.
           10  D1-QTITLO-POSIC-CONSL   PIC S9(015)      COMP-3.
           10  D1-PFAIXA-POSIC-CONSL   PIC S9(003)V9(2) COMP-3.
           10  D1-CSGMTO-CLI           PIC S9(003)      COMP-3.
           10  D1-VPSENT-PCELA-DESC    PIC S9(015)V9(2) COMP-3.
           10  D1-VRAP-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D1-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D1-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D1-RETORNO.
       05  D1-COD-RETORNO              PIC  9(004).
       05  D1-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D1-PROXIMO-RESTART          PIC  9(005).
       05  D1-QTDE-RETORNADA           PIC  9(003).
       05  D1-QTDE-TOTAL               PIC  9(009).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D1-OCORRENCIAS       OCCURS 070 TIMES INDEXED BY IND-TD1.
           10  D1RT-DMES-CONSL-PTCAO   PIC S9(002) COMP-3.
           10  D1RT-DPOSIC-CONSL-DESC  PIC  X(010).
           10  D1RT-CINDCD-TPO-CONSL   PIC S9(001) COMP-3.
           10  D1RT-CBCO               PIC S9(003) COMP-3.
           10  D1RT-CAG-BCRIA          PIC S9(005) COMP-3.
           10  D1RT-CCTA-BCRIA-CLI     PIC S9(013) COMP-3.
           10  D1RT-CPRODT             PIC S9(003) COMP-3.
           10  D1RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  D1RT-CFAIXA-PRZ-CONSL   PIC S9(003) COMP-3.
           10  D1RT-CINDCD-PER-CONSL   PIC  X(001).
           10  D1RT-CORIGE-OPER-DESC   PIC S9(001)      COMP-3.
           10  D1RT-VPRINC-PCELA-CONSL PIC S9(015)V9(2) COMP-3.
           10  D1RT-VENCAR-PCELA-CONSL PIC S9(015)V9(2) COMP-3.
           10  D1RT-VTOT-POSIC-CONSL   PIC S9(015)V9(2) COMP-3.
           10  D1RT-QTITLO-POSIC-CONSL PIC S9(015)      COMP-3.
           10  D1RT-PFAIXA-POSIC-CONSL PIC S9(003)V9(2) COMP-3.
           10  D1RT-CSGMTO-CLI         PIC S9(003)      COMP-3.
           10  D1RT-VPSENT-PCELA-DESC  PIC S9(015)V9(2) COMP-3.
           10  D1RT-VRAP-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
