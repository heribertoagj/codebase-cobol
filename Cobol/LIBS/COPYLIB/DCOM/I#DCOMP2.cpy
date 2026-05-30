      *================================================================*
      * INCLUDE PARA TABELA: DCOMB0P2 - DB2PRD.TPARM_LIBRC_DESC_SGMTO  *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM7735 - SELECT         - DCOM7736 - FETCH              *
      *    - DCOM7737 - INSERT         - DCOM7738 - UPDATE             *
      *    - DCOM7739 - DELETE                                         *
      *================================================================*
       01  P2-ARGUMENTOS-ENTRADA.

       05  P2-COLUNAS-DA-TABELA.
           10  P2-CPRODT               PIC S9(003) COMP-3.
           10  P2-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  P2-CSGMTO-CLI           PIC S9(003) COMP-3.
           10  P2-CORIGE-OPER-DESC     PIC S9(001) COMP-3.
           10  P2-HINIC-VGCIA-DESC     PIC  X(026).
           10  P2-HFIM-VGCIA-DESC      PIC  X(026).
           10  P2-VLIM-DESC-COML       PIC S9(015)V9(2) COMP-3.
           10  P2-HULT-ATULZ           PIC  X(026).
           10  P2-CFUNC-BDSCO          PIC S9(009) COMP-3.
           10  P2-CTERM                PIC  X(008).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  P2-QTDE-A-DESPREZAR         PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  P2-INSTRUCAO                PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  P2-RETORNO.
       05  P2-COD-RETORNO              PIC  9(004).
       05  P2-MENSAGEM                 PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  P2-PROXIMO-RESTART          PIC  9(005).
       05  P2-QTDE-RETORNADA           PIC  9(003).
       05  P2-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  P2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TP2.
           10  P2RT-CPRODT             PIC S9(003) COMP-3.
           10  P2RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  P2RT-CSGMTO-CLI         PIC S9(003) COMP-3.
           10  P2RT-CORIGE-OPER-DESC   PIC S9(001) COMP-3.
           10  P2RT-HINIC-VGCIA-DESC   PIC  X(026).
           10  P2RT-HFIM-VGCIA-DESC    PIC  X(026).
           10  P2RT-VLIM-DESC-COML     PIC S9(015)V9(2) COMP-3.
           10  P2RT-HULT-ATULZ         PIC  X(026).
           10  P2RT-CFUNC-BDSCO        PIC S9(009) COMP-3.
           10  P2RT-CTERM              PIC  X(008).
