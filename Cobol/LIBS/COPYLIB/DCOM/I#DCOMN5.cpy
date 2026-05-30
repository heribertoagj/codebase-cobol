      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N5 - DB2PRD.TANLSE_RCBVL_DESC      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7655 - SELECT                                        *
      *    - DCOM7656 - FETCH                                         *
      *    - DCOM7657 - INSERT                                        *
      *    - DCOM7658 - UPDATE                                        *
      *    - DCOM7659 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  N5-ARGUMENTOS-ENTRADA.

       05  N5-COLUNAS-DA-TABELA.
           10  N5-CBCO                 PIC S9(003) COMP-3.
           10  N5-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N5-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N5-CELMTO-DESC-COML     PIC S9(003) COMP-3.
           10  N5-CCHAVE-ELMTO-DESC    PIC  X(045).
           10  N5-CPTCAO-TBELA         PIC S9(003) COMP-3.
           10  N5-CPRODT               PIC S9(003) COMP-3.
           10  N5-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N5-CRESUL-ANLSE-TITLO   PIC S9(002) COMP-3.
           10  N5-DSOLTC-ANLSE-CREDT   PIC  X(010).
           10  N5-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N5-CUSUAR-INCL          PIC  X(009).
           10  N5-HINCL-REG            PIC  X(026).
           10  N5-CUSUAR-MANUT         PIC  X(009).
           10  N5-HULT-ATULZ           PIC  X(026).
           10  N5-CTERM                PIC  X(008).
           10  N5-WRESUL-ANLSE-TITLO-LEN  PIC S9(004).
           10  N5-WRESUL-ANLSE-TITLO-TEXT PIC  X(2000).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N5-QTDE-A-DESPREZAR         PIC  9(005).
       05  N5-START-COUNT              PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, OBRIGATORIO
      *-----------------------------------------------*
       05  N5-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N5-RETORNO.
       05  N5-COD-RETORNO              PIC  9(04).
       05  N5-MENSAGEM                 PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N5-PROXIMO-RESTART          PIC  9(05).
       05  N5-QTDE-RETORNADA           PIC  9(03).
       05  N5-QTDE-TOTAL               PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TN5.
           10  N5RT-CBCO                 PIC S9(003) COMP-3.
           10  N5RT-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N5RT-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N5RT-CELMTO-DESC-COML     PIC S9(003) COMP-3.
           10  N5RT-CCHAVE-ELMTO-DESC    PIC  X(045).
           10  N5RT-CPTCAO-TBELA         PIC S9(003) COMP-3.
           10  N5RT-CPRODT               PIC S9(003) COMP-3.
           10  N5RT-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N5RT-CRESUL-ANLSE-TITLO   PIC S9(002) COMP-3.
           10  N5RT-DSOLTC-ANLSE-CREDT   PIC  X(010).
           10  N5RT-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N5RT-CUSUAR-INCL          PIC  X(009).
           10  N5RT-HINCL-REG            PIC  X(026).
           10  N5RT-CUSUAR-MANUT         PIC  X(009).
           10  N5RT-HULT-ATULZ           PIC  X(026).
           10  N5RT-CTERM                PIC  X(008).
           10  N5RT-WRESUL-ANLSE-TITLO-LEN  PIC S9(004).
           10  N5RT-WRESUL-ANLSE-TITLO-TEXT PIC  X(2000).

