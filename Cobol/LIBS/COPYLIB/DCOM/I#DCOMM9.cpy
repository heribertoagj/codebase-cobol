      *================================================================*
      * INCLUDE PARA TABELA:                                           *
      *    - DCOMB0M9 - DB2PRD.TROTRO_CTBIL_PREJ                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM7625 - SELECT         - DCOM7626 - FETCH              *
      *    - DCOM7627 - INSERT         - DCOM7628 - UPDATE             *
      *    - DCOM7629 - DELETE                                         *
      *----------------------------------------------------------------*
      * FORMATACAO.........:                                           *
      *    - M9-CINDCD-PSSOA-TOMAD (TIPO DE PESSOA)                    *
      *       - F = FISICA         - J = JURIDICA       - A = AMBAS    *
      *       - G = AGRICULTURA    - P = PECUARIA                      *
      *================================================================*

      ******************************************************************
      * AREA DE ENTRADA                                                *
      ******************************************************************

       01  M9-ARGUMENTOS-ENTRADA.
           05 M9-COLUNAS-DA-TABELA.
              10 M9-CPRODT             PIC S9(003)V COMP-3.
              10 M9-CSPROD-DESC-COML   PIC S9(003)V COMP-3.
              10 M9-CFATO-GERDR-CTBIL  PIC S9(003)V COMP-3.
              10 M9-CINDCD-PSSOA-TOMAD PIC  X(001).
              10 M9-CMEIO-ENTRD-DESC   PIC S9(003)V COMP-3.
              10 M9-CSIT-DESC-COML     PIC S9(003)V COMP-3.
              10 M9-CMOTVO-BAIXA-PREJ  PIC S9(003)V COMP-3.
              10 M9-CREGRA-CTBIL-PREJ  PIC S9(003)V COMP-3.
              10 M9-COPER-CTBIL        PIC  X(007).
              10 M9-CPRODT-CTBIL-ELEMT PIC  X(004).
              10 M9-HINIC-VGCIA-TRILA  PIC  X(026).
              10 M9-HULT-ATULZ         PIC  X(026).
              10 M9-CFUNC-BDSCO        PIC S9(009)V COMP-3.
              10 M9-CTERM              PIC  X(008).
              10 M9-HFIM-VGCIA-TRILA   PIC  X(026).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *----------------------------------------------------------------*
           05 M9-QTDE-A-DESPREZAR      PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *----------------------------------------------------------------*
           05 M9-INSTRUCAO             PIC X(010).


      ******************************************************************
      * AREA DE RETORNO                                                *
      ******************************************************************

       01  M9-RETORNO.
           05 M9-COD-RETORNO           PIC  9(004).
           05 M9-MENSAGEM              PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *----------------------------------------------------------------*
           05 M9-PROXIMO-RESTART       PIC  9(005).
           05 M9-QTDE-RETORNADA        PIC  9(003).
           05 M9-QTDE-TOTAL            PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *----------------------------------------------------------------*
           05 M9-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TM9.
              10 M9RT-CPRODT              PIC S9(003)V COMP-3.
              10 M9RT-CSPROD-DESC-COML    PIC S9(003)V COMP-3.
              10 M9RT-CFATO-GERDR-CTBIL   PIC S9(003)V COMP-3.
              10 M9RT-CINDCD-PSSOA-TOMAD  PIC  X(001).
              10 M9RT-CMEIO-ENTRD-DESC    PIC S9(003)V COMP-3.
              10 M9RT-CSIT-DESC-COML      PIC S9(003)V COMP-3.
              10 M9RT-CMOTVO-BAIXA-PREJ   PIC S9(003)V COMP-3.
              10 M9RT-CREGRA-CTBIL-PREJ   PIC S9(003)V COMP-3.
              10 M9RT-COPER-CTBIL         PIC  X(007).
              10 M9RT-CPRODT-CTBIL-ELEMT  PIC  X(004).
              10 M9RT-HINIC-VGCIA-TRILA   PIC  X(026).
              10 M9RT-HULT-ATULZ          PIC  X(026).
              10 M9RT-CFUNC-BDSCO         PIC S9(009)V COMP-3.
              10 M9RT-CTERM               PIC  X(008).
              10 M9RT-HFIM-VGCIA-TRILA    PIC  X(026).
