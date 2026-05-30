      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N7 - DB2PRD.THIST_LIBRC_CLI        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7665 - SELECT                                        *
      *    - DCOM7666 - FETCH                                         *
      *    - DCOM7667 - INSERT                                        *
      *    - DCOM7668 - UPDATE                                        *
      *    - DCOM7669 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  N7-ARGUMENTOS-ENTRADA.

       05  N7-COLUNAS-DA-TABELA.
           10  N7-CBCO                 PIC S9(003) COMP-3.
           10  N7-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N7-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N7-CPRODT               PIC S9(003) COMP-3.
           10  N7-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N7-CCHAVE-ELMTO-DESC    PIC  X(045).
           10  N7-HINIC-VGCIA-DESC     PIC  X(026).
           10  N7-HFIM-VGCIA-DESC      PIC  X(026).
           10  N7-CELMTO-DESC-COML     PIC S9(003) COMP-3.
           10  N7-CINDCD-LIBRC-TITLO   PIC  X(001).
           10  N7-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N7-CUSUAR-INCL          PIC  X(009).
           10  N7-HINCL-REG            PIC  X(026).
           10  N7-CUSUAR-MANUT         PIC  X(009).
           10  N7-HULT-ATULZ           PIC  X(026).
           10  N7-CTERM                PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N7-QTDE-A-DESPREZAR         PIC  9(005).
       05  N7-START-COUNT              PIC  X(001).

      * PREENCHER PARA PARCIAL-03 ----------*
       05  N7-DINCL-REG-INI            PIC  X(010).
       05  N7-DINCL-REG-FIM            PIC  X(010).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  N7-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N7-RETORNO.
       05  N7-COD-RETORNO              PIC  9(04).
       05  N7-MENSAGEM                 PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N7-PROXIMO-RESTART          PIC  9(05).
       05  N7-QTDE-RETORNADA           PIC  9(03).
       05  N7-QTDE-TOTAL               PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N7-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TN7.
           10  N7RT-CBCO               PIC S9(003) COMP-3.
           10  N7RT-CAG-BCRIA          PIC S9(005) COMP-3.
           10  N7RT-CCTA-BCRIA-CLI     PIC S9(013) COMP-3.
           10  N7RT-CPRODT             PIC S9(003) COMP-3.
           10  N7RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  N7RT-CCHAVE-ELMTO-DESC  PIC  X(045).
           10  N7RT-HINIC-VGCIA-DESC   PIC  X(026).
           10  N7RT-HFIM-VGCIA-DESC    PIC  X(026).
           10  N7RT-CELMTO-DESC-COML   PIC S9(003) COMP-3.
           10  N7RT-CINDCD-LIBRC-TITLO PIC  X(001).
           10  N7RT-CIDTFD-TPO-MANUT   PIC  X(001).
           10  N7RT-CUSUAR-INCL        PIC  X(009).
           10  N7RT-HINCL-REG          PIC  X(026).
           10  N7RT-CUSUAR-MANUT       PIC  X(009).
           10  N7RT-HULT-ATULZ         PIC  X(026).
           10  N7RT-CTERM              PIC  X(008).
