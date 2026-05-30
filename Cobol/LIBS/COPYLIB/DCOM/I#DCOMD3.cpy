      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0D3 - DB2PRD.TPROVS_INADP_SEGMT                     *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7165 - SELECT         - DCOM7166 - FETCH             *
      *    - DCOM7167 - INSERT         - DCOM7168 - UPDATE            *
      *    - DCOM7169 - DELETE                                        *
      *===============================================================*

       01  D3-ARGUMENTOS-ENTRADA.

       05  D3-COLUNAS-DA-TABELA.
           10  D3-DPOSIC-CONSL-DESC    PIC  X(010).
           10  D3-CINDCD-TPO-CONSL     PIC S9(001) COMP-3.
           10  D3-CBCO                 PIC S9(003) COMP-3.
           10  D3-CAG-BCRIA            PIC S9(005) COMP-3.
           10  D3-CSGMTO-CLI           PIC S9(003) COMP-3.
           10  D3-CPRODT               PIC S9(003) COMP-3.
           10  D3-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  D3-CFAIXA-PRZ-CONSL     PIC S9(003) COMP-3.
           10  D3-CINDCD-PER-CONSL     PIC  X(001).
           10  D3-CORIGE-OPER-DESC     PIC S9(001)      COMP-3.
           10  D3-VPRINC-PCELA-CONSL   PIC S9(015)V9(2) COMP-3.
           10  D3-VENCAR-PCELA-CONSL   PIC S9(015)V9(2) COMP-3.
           10  D3-VTOT-POSIC-CONSL     PIC S9(015)V9(2) COMP-3.
           10  D3-QTITLO-POSIC-CONSL   PIC S9(015)      COMP-3.
           10  D3-PFAIXA-POSIC-CONSL   PIC S9(003)V9(2) COMP-3.
           10  D3-VPSENT-PCELA-DESC    PIC S9(015)V9(2) COMP-3.
           10  D3-VRAP-SDO-ATUAL       PIC S9(015)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D3-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D3-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D3-RETORNO.
       05  D3-COD-RETORNO              PIC  9(004).
       05  D3-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D3-PROXIMO-RESTART          PIC  9(005).
       05  D3-QTDE-RETORNADA           PIC  9(003).
       05  D3-QTDE-TOTAL               PIC  9(009).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D3-OCORRENCIAS       OCCURS 070 TIMES INDEXED BY IND-TD3.
           10  D3RT-DPOSIC-CONSL-DESC  PIC  X(010).
           10  D3RT-CINDCD-TPO-CONSL   PIC S9(001) COMP-3.
           10  D3RT-CBCO               PIC S9(003) COMP-3.
           10  D3RT-CAG-BCRIA          PIC S9(005) COMP-3.
           10  D3RT-CSGMTO-CLI         PIC S9(003) COMP-3.
           10  D3RT-CPRODT             PIC S9(003) COMP-3.
           10  D3RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  D3RT-CFAIXA-PRZ-CONSL   PIC S9(003) COMP-3.
           10  D3RT-CINDCD-PER-CONSL   PIC  X(001).
           10  D3RT-CORIGE-OPER-DESC   PIC S9(001)      COMP-3.
           10  D3RT-VPRINC-PCELA-CONSL PIC S9(015)V9(2) COMP-3.
           10  D3RT-VENCAR-PCELA-CONSL PIC S9(015)V9(2) COMP-3.
           10  D3RT-VTOT-POSIC-CONSL   PIC S9(015)V9(2) COMP-3.
           10  D3RT-QTITLO-POSIC-CONSL PIC S9(015)      COMP-3.
           10  D3RT-PFAIXA-POSIC-CONSL PIC S9(003)V9(2) COMP-3.
           10  D3RT-VPSENT-PCELA-DESC  PIC S9(015)V9(2) COMP-3.
           10  D3RT-VRAP-SDO-ATUAL     PIC S9(015)V9(2) COMP-3.
