      *================================================================*
      * INCLUDE PARA TABELA:                                           *
      *    - DCOMB055 - DB2PRD.TROTRO_TRILA_CTBIL                      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM6775 - SELECT         - DCOM6776 - FETCH              *
      *    - DCOM6777 - INSERT         - DCOM6778 - UPDATE             *
      *    - DCOM6779 - DELETE                                         *
      *----------------------------------------------------------------*
      * FORMATACAO.........:                                           *
      *    - 55-CINDCD-PSSOA-TOMAD (TIPO DE PESSOA)                    *
      *       - F = FISICA         - J = JURIDICA       - A = AMBAS    *
      *       - G = AGRICULTURA    - P = PECUARIA                      *
DTS001*----------------------------------------------------------------*
DTS001* ALTERACOES.........:                                           *
DTS001*    - 29/11/2011 - INCLUIR VIGENCIA PARA A TRILHA CONTABIL      *
DTS001*================================================================*

      ******************************************************************
      * AREA DE ENTRADA                                                *
      ******************************************************************

       01  55-ARGUMENTOS-ENTRADA.
           05 55-COLUNAS-DA-TABELA.
              10 55-CPRODT             PIC S9(003)V COMP-3.
              10 55-CSPROD-DESC-COML   PIC S9(003)V COMP-3.
              10 55-CFATO-GERDR-CTBIL  PIC S9(003)V COMP-3.
              10 55-CINDCD-PSSOA-TOMAD PIC  X(001).
              10 55-CMEIO-ENTRD-DESC   PIC S9(003)V COMP-3.
              10 55-CSIT-DESC-COML     PIC S9(003)V COMP-3.
              10 55-CPRODT-CTBIL-ELEMT PIC  X(004).
              10 55-COPER-CTBIL        PIC  X(007).
DTS001        10 55-HINIC-VGCIA-TRILA  PIC  X(026).
              10 55-HULT-ATULZ         PIC  X(026).
              10 55-CFUNC-BDSCO        PIC S9(009)V COMP-3.
              10 55-CTERM              PIC  X(008).
DTS001        10 55-HFIM-VGCIA-TRILA   PIC  X(026).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *----------------------------------------------------------------*
           05 55-QTDE-A-DESPREZAR      PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *----------------------------------------------------------------*
           05 55-INSTRUCAO             PIC X(010).


      ******************************************************************
      * AREA DE RETORNO                                                *
      ******************************************************************

       01  55-RETORNO.
           05 55-COD-RETORNO           PIC  9(004).
           05 55-MENSAGEM              PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *----------------------------------------------------------------*
           05 55-PROXIMO-RESTART       PIC  9(005).
           05 55-QTDE-RETORNADA        PIC  9(003).
           05 55-QTDE-TOTAL            PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *----------------------------------------------------------------*
           05 55-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-T55.
              10 55RT-CPRODT              PIC S9(003)V COMP-3.
              10 55RT-CSPROD-DESC-COML    PIC S9(003)V COMP-3.
              10 55RT-CFATO-GERDR-CTBIL   PIC S9(003)V COMP-3.
              10 55RT-CINDCD-PSSOA-TOMAD  PIC  X(001).
              10 55RT-CMEIO-ENTRD-DESC    PIC S9(003)V COMP-3.
              10 55RT-CSIT-DESC-COML      PIC S9(003)V COMP-3.
              10 55RT-CPRODT-CTBIL-ELEMT  PIC  X(004).
              10 55RT-COPER-CTBIL         PIC  X(007).
DTS001        10 55RT-HINIC-VGCIA-TRILA   PIC  X(026).
              10 55RT-HULT-ATULZ          PIC  X(026).
              10 55RT-CFUNC-BDSCO         PIC S9(009)V COMP-3.
              10 55RT-CTERM               PIC  X(008).
DTS001        10 55RT-HFIM-VGCIA-TRILA    PIC  X(026).
