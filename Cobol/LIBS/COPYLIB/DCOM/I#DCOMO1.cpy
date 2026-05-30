      *================================================================*
      * INCLUDE PARA TABELA: DCOMB001 - DB2PRD.TROTRO_CTBIL_PGTO       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM7690 - SELECT         - DCOM7691 - FETCH              *
      *    - DCOM7692 - INSERT         - DCOM7693 - UPDATE             *
      *    - DCOM7694 - DELETE                                         *
      *================================================================*

       01  O1-ARGUMENTOS-ENTRADA.

       05  O1-COLUNAS-DA-TABELA.
           10  O1-CPRODT               PIC S9(003) COMP-3.
           10  O1-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  O1-CFATO-GERDR-CTBIL    PIC S9(003) COMP-3.
           10  O1-CINDCD-PSSOA-TOMAD   PIC  X(001).
           10  O1-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
           10  O1-CORIGE-PGTO-DESC     PIC S9(003) COMP-3.
           10  O1-HINIC-VGCIA-TRILA    PIC  X(026).
           10  O1-CPRODT-CTBIL-ELEMT   PIC  X(004).
           10  O1-COPER-CTBIL          PIC  X(007).
           10  O1-CGRP-CTBIL-ATUAL     PIC S9(003) COMP-3.
           10  O1-CSGRP-CTBIL          PIC S9(003) COMP-3.
           10  O1-CCTA-CTBIL           PIC S9(007) COMP-3.
           10  O1-HFIM-VGCIA-TRILA     PIC  X(026).
           10  O1-HULT-ATULZ           PIC  X(026).
           10  O1-CFUNC-BDSCO          PIC S9(009) COMP-3.
           10  O1-CTERM                PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  O1-QTDE-A-DESPREZAR         PIC  9(005).
       05  O1-START-COUNT              PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  O1-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  O1-RETORNO.
       05  O1-COD-RETORNO              PIC  9(004).
       05  O1-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  O1-PROXIMO-RESTART          PIC  9(005).
       05  O1-QTDE-RETORNADA           PIC  9(003).
       05  O1-QTDE-TOTAL               PIC  9(009).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  O1-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TO1.
           10  O1RT-CPRODT             PIC S9(003) COMP-3.
           10  O1RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  O1RT-CFATO-GERDR-CTBIL  PIC S9(003) COMP-3.
           10  O1RT-CINDCD-PSSOA-TOMAD PIC  X(001).
           10  O1RT-CMEIO-ENTRD-DESC   PIC S9(003) COMP-3.
           10  O1RT-CORIGE-PGTO-DESC   PIC S9(003) COMP-3.
           10  O1RT-HINIC-VGCIA-TRILA  PIC  X(026).
           10  O1RT-CPRODT-CTBIL-ELEMT PIC  X(004).
           10  O1RT-COPER-CTBIL        PIC  X(007).
           10  O1RT-CGRP-CTBIL-ATUAL   PIC S9(003) COMP-3.
           10  O1RT-CSGRP-CTBIL        PIC S9(003) COMP-3.
           10  O1RT-CCTA-CTBIL         PIC S9(007) COMP-3.
           10  O1RT-HFIM-VGCIA-TRILA   PIC  X(026).
           10  O1RT-HULT-ATULZ         PIC  X(026).
           10  O1RT-CFUNC-BDSCO        PIC S9(009) COMP-3.
           10  O1RT-CTERM              PIC  X(008).

