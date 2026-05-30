      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0P0 - DB2PRD.TPARM_LIM_SPROD        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7675 - SELECT                                        *
      *    - DCOM7676 - FETCH                                         *
      *    - DCOM7677 - INSERT                                        *
      *    - DCOM7678 - UPDATE                                        *
      *    - DCOM7679 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *---------------------------------------------------------------*
      * CIDTFD-PARM-SPROD > 1-PRAZO TITULOS EM ALERTA                 *
      *                     2-VALOR LIMITE TITULOS                    *
      *****************************************************************
       01  P0-ARGUMENTOS-ENTRADA.

       05  P0-COLUNAS-DA-TABELA.
           10  P0-CPRODT               PIC S9(003) COMP-3.
           10  P0-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  P0-CSGMTO-CLI           PIC S9(003) COMP-3.
           10  P0-CORIGE-OPER-DESC     PIC S9(001) COMP-3.
           10  P0-CIDTFD-PARM-SPROD    PIC S9(001) COMP-3.
           10  P0-HINIC-VGCIA-DESC     PIC  X(026).
           10  P0-HFIM-VGCIA-DESC      PIC  X(026).
           10  P0-QDIA-PRZ-LIM         PIC S9(005) COMP-3.
           10  P0-VLIM-MIN-DESC        PIC S9(15)V9(2) COMP-3.
           10  P0-CINDCD-PRMSS-TITLO   PIC  X(001).
           10  P0-CIDTFD-TPO-MANUT     PIC  X(001).
           10  P0-CUSUAR-INCL          PIC  X(009).
           10  P0-HINCL-REG            PIC  X(026).
           10  P0-CUSUAR-MANUT         PIC  X(009).
           10  P0-HULT-ATULZ           PIC  X(026).
           10  P0-CTERM                PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  P0-QTDE-A-DESPREZAR         PIC  9(005).
       05  P0-START-COUNT              PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  P0-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  P0-RETORNO.
       05  P0-COD-RETORNO              PIC  9(04).
       05  P0-MENSAGEM                 PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  P0-PROXIMO-RESTART          PIC  9(05).
       05  P0-QTDE-RETORNADA           PIC  9(03).
       05  P0-QTDE-TOTAL               PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  P0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TP0.
           10  P0RT-CPRODT               PIC S9(003) COMP-3.
           10  P0RT-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  P0RT-CSGMTO-CLI           PIC S9(003) COMP-3.
           10  P0RT-CORIGE-OPER-DESC     PIC S9(001) COMP-3.
           10  P0RT-CIDTFD-PARM-SPROD    PIC S9(001) COMP-3.
           10  P0RT-HINIC-VGCIA-DESC     PIC  X(026).
           10  P0RT-HFIM-VGCIA-DESC      PIC  X(026).
           10  P0RT-QDIA-PRZ-LIM         PIC S9(005) COMP-3.
           10  P0RT-VLIM-MIN-DESC        PIC S9(15)V9(2) COMP-3.
           10  P0RT-CINDCD-PRMSS-TITLO   PIC  X(001).
           10  P0RT-CIDTFD-TPO-MANUT     PIC  X(001).
           10  P0RT-CUSUAR-INCL          PIC  X(009).
           10  P0RT-HINCL-REG            PIC  X(026).
           10  P0RT-CUSUAR-MANUT         PIC  X(009).
           10  P0RT-HULT-ATULZ           PIC  X(026).
           10  P0RT-CTERM                PIC  X(008).

