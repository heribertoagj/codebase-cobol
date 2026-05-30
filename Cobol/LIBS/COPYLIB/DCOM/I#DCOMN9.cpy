      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N9 - DB2PRD.TPARM_LIM_CLI          *
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
      *****************************************************************
       01  N9-ARGUMENTOS-ENTRADA.

       05  N9-COLUNAS-DA-TABELA.
           10  N9-CBCO                 PIC S9(003) COMP-3.
           10  N9-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N9-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N9-CPRODT               PIC S9(003) COMP-3.
           10  N9-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N9-HINIC-VGCIA-DESC     PIC  X(026).
           10  N9-HFIM-VGCIA-DESC      PIC  X(026).
           10  N9-VLIM-MIN-DESC        PIC S9(15)V9(2) COMP-3.
           10  N9-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N9-HINCL-REG            PIC  X(026).
           10  N9-CUSUAR-INCL          PIC  X(009).
           10  N9-HULT-ATULZ           PIC  X(026).
           10  N9-CUSUAR-MANUT         PIC  X(009).
           10  N9-CTERM                PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N9-QTDE-A-DESPREZAR         PIC  9(005).
       05  N9-START-COUNT              PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  N9-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N9-RETORNO.
       05  N9-COD-RETORNO              PIC  9(04).
       05  N9-MENSAGEM                 PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N9-PROXIMO-RESTART          PIC  9(05).
       05  N9-QTDE-RETORNADA           PIC  9(03).
       05  N9-QTDE-TOTAL               PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TN9.
           10  N9RT-CBCO                 PIC S9(003) COMP-3.
           10  N9RT-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N9RT-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N9RT-CPRODT               PIC S9(003) COMP-3.
           10  N9RT-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N9RT-HINIC-VGCIA-DESC     PIC  X(026).
           10  N9RT-HFIM-VGCIA-DESC      PIC  X(026).
           10  N9RT-VLIM-MIN-DESC        PIC S9(15)V9(2) COMP-3.
           10  N9RT-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N9RT-HINCL-REG            PIC  X(026).
           10  N9RT-CUSUAR-INCL          PIC  X(009).
           10  N9RT-HULT-ATULZ           PIC  X(026).
           10  N9RT-CUSUAR-MANUT         PIC  X(009).
           10  N9RT-CTERM                PIC  X(008).

