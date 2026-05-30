      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N8 - DB2PRD.TLIBRC_CLI_RCBVL       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7670 - SELECT                                        *
      *    - DCOM7671 - FETCH                                         *
      *    - DCOM7672 - INSERT                                        *
      *    - DCOM7673 - UPDATE                                        *
      *    - DCOM7674 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  N8-ARGUMENTOS-ENTRADA.

       05  N8-COLUNAS-DA-TABELA.
           10  N8-CBCO                 PIC S9(003) COMP-3.
           10  N8-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N8-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N8-CPRODT               PIC S9(003) COMP-3.
           10  N8-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N8-CCHAVE-ELMTO-DESC    PIC  X(045).
           10  N8-HINIC-VGCIA-DESC     PIC  X(026).
           10  N8-HFIM-VGCIA-DESC      PIC  X(026).
           10  N8-CELMTO-DESC-COML     PIC S9(003) COMP-3.
           10  N8-CINDCD-LIBRC-TITLO   PIC  X(001).
           10  N8-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N8-CUSUAR-INCL          PIC  X(009).
           10  N8-HINCL-REG            PIC  X(026).
           10  N8-CUSUAR-MANUT         PIC  X(009).
           10  N8-HULT-ATULZ           PIC  X(026).
           10  N8-CTERM                PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N8-QTDE-A-DESPREZAR         PIC  9(005).
       05  N8-START-COUNT              PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  N8-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N8-RETORNO.
       05  N8-COD-RETORNO              PIC  9(04).
       05  N8-MENSAGEM                 PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N8-PROXIMO-RESTART          PIC  9(05).
       05  N8-QTDE-RETORNADA           PIC  9(03).
       05  N8-QTDE-TOTAL               PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TN8.
           10  N8RT-CBCO               PIC S9(003) COMP-3.
           10  N8RT-CAG-BCRIA          PIC S9(005) COMP-3.
           10  N8RT-CCTA-BCRIA-CLI     PIC S9(013) COMP-3.
           10  N8RT-CPRODT             PIC S9(003) COMP-3.
           10  N8RT-CSPROD-DESC-COML   PIC S9(003) COMP-3.
           10  N8RT-CCHAVE-ELMTO-DESC  PIC  X(045).
           10  N8RT-HINIC-VGCIA-DESC   PIC  X(026).
           10  N8RT-HFIM-VGCIA-DESC    PIC  X(026).
           10  N8RT-CELMTO-DESC-COML   PIC S9(003) COMP-3.
           10  N8RT-CINDCD-LIBRC-TITLO PIC  X(001).
           10  N8RT-CIDTFD-TPO-MANUT   PIC  X(001).
           10  N8RT-CUSUAR-INCL        PIC  X(009).
           10  N8RT-HINCL-REG          PIC  X(026).
           10  N8RT-CUSUAR-MANUT       PIC  X(009).
           10  N8RT-HULT-ATULZ         PIC  X(026).
           10  N8RT-CTERM              PIC  X(008).

