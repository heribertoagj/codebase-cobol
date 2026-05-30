      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  64-ARGUMENTOS-ENTRADA.

       05  64-COLUNAS-DA-TABELA.
           10  64-CPRODT                   PIC S9(3)V COMP-3.
           10  64-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  64-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  64-HINIC-MEIO-ENTRD         PIC X(26).
           10  64-CINDCD-MODLD-DESC        PIC X(1).
           10  64-HFIM-MEIO-ENTRD          PIC X(26).
           10  64-CSIT-DESC-COML           PIC S9(3)V COMP-3.
           10  64-HSIT-DESC-COML           PIC X(26).
           10  64-CINDCD-FORML-OPER        PIC X(1).
           10  64-CINDCD-LIBRC-OPER        PIC X(1).
           10  64-CINDCD-LIBRC-ANTCP       PIC X(1).
           10  64-PMAX-LIBRC-ANTCP         PIC S9(3)V9(3) COMP-3.
           10  64-TMIN-VCTO-TITLO          PIC S9(5)V COMP-3.
           10  64-TMAX-VCTO-TITLO          PIC S9(5)V COMP-3.
           10  64-CINDCD-PRZ-VCTO          PIC X(2).
           10  64-TMAX-ACLTO-TITLO         PIC S9(3)V COMP-3.
           10  64-CINDCD-PRZ-ACLTO         PIC X(2).
           10  64-VMAX-DIVRG-TITLO         PIC S9(15)V9(2) COMP-3.
           10  64-CINDCD-OBRIG-GARNT       PIC X(1).
           10  64-CINDCD-OBRIG-PROMS       PIC X(1).
           10  64-CCART-NORML-DESC         PIC X(5).
           10  64-CCART-VENCD-DESC         PIC X(5).
           10  64-CCART-DESC-PRODT         PIC X(5).
           10  64-CINDCD-TARIF-TRNSF       PIC X(1).
           10  64-VTARIF-TRNSF-CART        PIC S9(15)V9(2) COMP-3.
           10  64-CTARIF-TRNSF-CART        PIC S9(5)V COMP-3.
           10  64-CCOBR-TAC-OPER           PIC S9(3)V COMP-3.
           10  64-VTAC-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  64-CTAC-OPER-DESC           PIC S9(5)V COMP-3.
           10  64-CTAC-PERC-OPER           PIC S9(5)V COMP-3.
           10  64-CTAC-MIN-OPER            PIC S9(5)V COMP-3.
           10  64-CTAC-MAX-OPER            PIC S9(5)V COMP-3.
           10  64-CCOBR-TAC-CONTR          PIC S9(3)V COMP-3.
           10  64-VTAC-OPER-EFETU          PIC S9(15)V9(2) COMP-3.
           10  64-CTAC-OPER-EFETU          PIC S9(5)V COMP-3.
           10  64-CTAC-PERC-CONTR          PIC S9(5)V COMP-3.
           10  64-CTAC-MIN-CONTR           PIC S9(5)V COMP-3.
           10  64-CTAC-MAX-CONTR           PIC S9(5)V COMP-3.
           10  64-HULT-ATULZ               PIC X(26).
           10  64-WCTRL-FLUXO-DIGTC        PIC X(15).
           10  64-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  64-CTERM                    PIC X(8).
           10  64-CINDCD-TARIF-REG         PIC X(01).
           10  64-CINDCD-PRZ-MAX           PIC X(02).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  64-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  64-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  64-RETORNO.
       05  64-COD-RETORNO                         PIC 9(04).
       05  64-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  64-PROXIMO-RESTART                     PIC 9(05).
       05  64-QTDE-RETORNADA                      PIC 9(03).
       05  64-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  64-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T64.
           10  64RT-CPRODT                   PIC S9(3)V COMP-3.
           10  64RT-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  64RT-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  64RT-HINIC-MEIO-ENTRD         PIC X(26).
           10  64RT-CINDCD-MODLD-DESC        PIC X(1).
           10  64RT-HFIM-MEIO-ENTRD          PIC X(26).
           10  64RT-CSIT-DESC-COML           PIC S9(3)V COMP-3.
           10  64RT-HSIT-DESC-COML           PIC X(26).
           10  64RT-CINDCD-FORML-OPER        PIC X(1).
           10  64RT-CINDCD-LIBRC-OPER        PIC X(1).
           10  64RT-CINDCD-LIBRC-ANTCP       PIC X(1).
           10  64RT-PMAX-LIBRC-ANTCP         PIC S9(3)V9(3) COMP-3.
           10  64RT-TMIN-VCTO-TITLO          PIC S9(5)V COMP-3.
           10  64RT-TMAX-VCTO-TITLO          PIC S9(5)V COMP-3.
           10  64RT-CINDCD-PRZ-VCTO          PIC X(2).
           10  64RT-TMAX-ACLTO-TITLO         PIC S9(3)V COMP-3.
           10  64RT-CINDCD-PRZ-ACLTO         PIC X(2).
           10  64RT-VMAX-DIVRG-TITLO         PIC S9(15)V9(2) COMP-3.
           10  64RT-CINDCD-OBRIG-GARNT       PIC X(1).
           10  64RT-CINDCD-OBRIG-PROMS       PIC X(1).
           10  64RT-CCART-NORML-DESC         PIC X(5).
           10  64RT-CCART-VENCD-DESC         PIC X(5).
           10  64RT-CCART-DESC-PRODT         PIC X(5).
           10  64RT-CINDCD-TARIF-TRNSF       PIC X(1).
           10  64RT-VTARIF-TRNSF-CART        PIC S9(15)V9(2) COMP-3.
           10  64RT-CTARIF-TRNSF-CART        PIC S9(5)V COMP-3.
           10  64RT-CCOBR-TAC-OPER           PIC S9(3)V COMP-3.
           10  64RT-VTAC-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  64RT-CTAC-OPER-DESC           PIC S9(5)V COMP-3.
           10  64RT-CTAC-PERC-OPER           PIC S9(5)V COMP-3.
           10  64RT-CTAC-MIN-OPER            PIC S9(5)V COMP-3.
           10  64RT-CTAC-MAX-OPER            PIC S9(5)V COMP-3.
           10  64RT-CCOBR-TAC-CONTR          PIC S9(3)V COMP-3.
           10  64RT-VTAC-OPER-EFETU          PIC S9(15)V9(2) COMP-3.
           10  64RT-CTAC-OPER-EFETU          PIC S9(5)V COMP-3.
           10  64RT-CTAC-PERC-CONTR          PIC S9(5)V COMP-3.
           10  64RT-CTAC-MIN-CONTR           PIC S9(5)V COMP-3.
           10  64RT-CTAC-MAX-CONTR           PIC S9(5)V COMP-3.
           10  64RT-HULT-ATULZ               PIC X(26).
           10  64RT-WCTRL-FLUXO-DIGTC        PIC X(15).
           10  64RT-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  64RT-CTERM                    PIC X(8).
           10  64RT-CINDCD-TARIF-REG         PIC X(01).
           10  64RT-CINDCD-PRZ-MAX           PIC X(02).
