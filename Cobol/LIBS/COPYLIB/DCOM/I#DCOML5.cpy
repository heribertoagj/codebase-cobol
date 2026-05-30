      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0L5 - DB2PRD.TPARM_SEQ_CART                         *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7555 - SELECT         - DCOM7556 - FETCH             *
      *    - DCOM7557 - INSERT         - DCOM7558 - UPDATE            *
      *    - DCOM7558 - DELETE                                        *
      *---------------------------------------------------------------*
      * FORMATACAO.........:                                          *
      *    - L5-CORIGE-OPER-DESC (ORIGEM DA OPERACAO - CANAL)         *
      *       -  1 = AGENCIA           - 2 = NETEMPRESA               *
      *       -  3 = TELEBANCO         - 4 = SHOPCREDIT               *
      *===============================================================*

       01  L5-ARGUMENTOS-ENTRADA.
           05 L5-COLUNAS-DA-TABELA.
              10 L5-CPRODT             PIC S9(003)V COMP-3.
              10 L5-CSPROD-DESC-COML   PIC S9(003)V COMP-3.
              10 L5-CORIGE-OPER-DESC   PIC S9(001)V COMP-3.
              10 L5-NSEQ-CART-LIM      PIC S9(003)V COMP-3.
              10 L5-CCART              PIC  X(005).
              10 L5-HULT-ATULZ         PIC  X(026).
              10 L5-CFUNC-BDSCO        PIC S9(009)V COMP-3.
              10 L5-CTERM              PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
           05 L5-QTDE-A-DESPREZAR      PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
           05 L5-INSTRUCAO             PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************

       01  L5-RETORNO.
           05 L5-COD-RETORNO           PIC  9(004).
           05 L5-MENSAGEM              PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
           05 L5-PROXIMO-RESTART       PIC  9(005).
           05 L5-QTDE-RETORNADA        PIC  9(003).
           05 L5-QTDE-TOTAL            PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
           05 L5-OCORRENCIAS    OCCURS 070 TIMES INDEXED BY IND-TL5.
              10 L5RT-CPRODT           PIC S9(003)V COMP-3.
              10 L5RT-CSPROD-DESC-COML PIC S9(003)V COMP-3.
              10 L5RT-CORIGE-OPER-DESC PIC S9(001)V COMP-3.
              10 L5RT-NSEQ-CART-LIM    PIC S9(003)V COMP-3.
              10 L5RT-CCART            PIC  X(005).
              10 L5RT-HULT-ATULZ       PIC  X(026).
              10 L5RT-CFUNC-BDSCO      PIC S9(009)V COMP-3.
              10 L5RT-CTERM            PIC  X(008).
