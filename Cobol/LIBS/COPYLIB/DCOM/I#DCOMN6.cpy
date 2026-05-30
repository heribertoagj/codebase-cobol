      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N6 - DB2PRD.THIST_ANLSE_RCBVL      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7660 - SELECT                                        *
      *    - DCOM7661 - FETCH                                         *
      *    - DCOM7662 - INSERT                                        *
      *    - DCOM7663 - UPDATE                                        *
      *    - DCOM7664 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  N6-ARGUMENTOS-ENTRADA.

       05  N6-COLUNAS-DA-TABELA.
           10  N6-CBCO                 PIC S9(003) COMP-3.
           10  N6-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N6-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N6-CELMTO-DESC-COML     PIC S9(003) COMP-3.
           10  N6-CCHAVE-ELMTO-DESC    PIC  X(045).
           10  N6-HINCL-REG-HIST       PIC  X(026).
           10  N6-CPTCAO-TBELA         PIC S9(003) COMP-3.
           10  N6-CPRODT               PIC S9(003) COMP-3.
           10  N6-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N6-CRESUL-ANLSE-TITLO   PIC S9(002) COMP-3.
           10  N6-DSOLTC-ANLSE-CREDT   PIC  X(010).
           10  N6-CINDCD-TITLO-DESC    PIC  X(001).
           10  N6-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N6-CUSUAR-INCL          PIC  X(009).
           10  N6-HINCL-REG            PIC  X(026).
           10  N6-CUSUAR-MANUT         PIC  X(009).
           10  N6-HULT-ATULZ           PIC  X(026).
           10  N6-CTERM                PIC  X(008).
           10  N6-WRESUL-ANLSE-TITLO-LEN  PIC S9(004).
           10  N6-WRESUL-ANLSE-TITLO-TEXT PIC  X(2000).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N6-QTDE-A-DESPREZAR         PIC  9(005).
       05  N6-START-COUNT              PIC  X(001).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  N6-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N6-RETORNO.
       05  N6-COD-RETORNO              PIC  9(04).
       05  N6-MENSAGEM                 PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N6-PROXIMO-RESTART          PIC  9(05).
       05  N6-QTDE-RETORNADA           PIC  9(03).
       05  N6-QTDE-TOTAL               PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TN6.
           10  N6RT-CBCO                 PIC S9(003) COMP-3.
           10  N6RT-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N6RT-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N6RT-CELMTO-DESC-COML     PIC S9(003) COMP-3.
           10  N6RT-CCHAVE-ELMTO-DESC    PIC  X(045).
           10  N6RT-HINCL-REG-HIST       PIC  X(026).
           10  N6RT-CPTCAO-TBELA         PIC S9(003) COMP-3.
           10  N6RT-CPRODT               PIC S9(003) COMP-3.
           10  N6RT-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N6RT-CRESUL-ANLSE-TITLO   PIC S9(002) COMP-3.
           10  N6RT-DSOLTC-ANLSE-CREDT   PIC  X(010).
           10  N6RT-CINDCD-TITLO-DESC    PIC  X(001).
           10  N6RT-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N6RT-CUSUAR-INCL          PIC  X(009).
           10  N6RT-HINCL-REG            PIC  X(026).
           10  N6RT-CUSUAR-MANUT         PIC  X(009).
           10  N6RT-HULT-ATULZ           PIC  X(026).
           10  N6RT-CTERM                PIC  X(008).
           10  N6RT-WRESUL-ANLSE-TITLO-LEN  PIC S9(004).
           10  N6RT-WRESUL-ANLSE-TITLO-TEXT PIC  X(2000).

