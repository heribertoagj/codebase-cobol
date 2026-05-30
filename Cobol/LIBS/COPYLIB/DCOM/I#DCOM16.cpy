      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB016 - DB2PRD.TCONVE_GRAL_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  16-ARGUMENTOS-ENTRADA.

       05  16-COLUNAS-DA-TABELA.
           10  16-CCONVE-GRAL-DESC             PIC S9(9)V COMP-3.
           10  16-CVRSAO-CONVE-GRAL            PIC S9(3)V COMP-3.
           10  16-ICONVE-GRAL-DESC             PIC X(40).
           10  16-IRSUMO-CONVE-GRAL            PIC X(15).
           10  16-CBCO-OPER-CONVE              PIC S9(3)V COMP-3.
           10  16-CPRODT                       PIC S9(3)V COMP-3.
           10  16-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  16-CTPO-DEPDC-EXCVD             PIC X(1).
           10  16-CINDCD-ABRGC-GEOGR           PIC X(1).
           10  16-DINCL-CONVE-GRAL             PIC X(10).
           10  16-DINIC-VGCIA-CONVE            PIC X(10).
           10  16-DVCTO-CONVE-DESC             PIC X(10).
           10  16-TCONVE-DESC-COML             PIC S9(5)V COMP-3.
           10  16-CINDCD-PRZ-CONVE             PIC X(2).
           10  16-VLIM-CONVE-GRAL              PIC S9(15)V9(2) COMP-3.
           10  16-VUTLZD-CONVE-GRAL            PIC S9(15)V9(2) COMP-3.
           10  16-VDISPN-CONVE-GRAL            PIC S9(15)V9(2) COMP-3.
           10  16-HATULZ-SDO-CONVE             PIC X(26).
           10  16-CINDCD-CONVE-ROTVO           PIC X(1).
           10  16-CTPO-COBR-TAC                PIC S9(3)V COMP-3.
           10  16-CIDTFD-FIXO-TAC              PIC S9(5)V COMP-3.
           10  16-CIDTFD-PERC-TAC              PIC S9(5)V COMP-3.
           10  16-CIDTFD-MIN-TAC               PIC S9(5)V COMP-3.
           10  16-CIDTFD-MAX-TAC               PIC S9(5)V COMP-3.
           10  16-TDEB-PCELA-VENCD             PIC S9(3)V COMP-3.
           10  16-CINDCD-PRZ-PCELA             PIC X(2).
           10  16-CINDCD-TARIF-REG             PIC X(1).
           10  16-CINDCD-PGDOR-JURO            PIC X(1).
           10  16-CINDCD-LIBRC-ANTCP           PIC X(1).
           10  16-PMAX-LIBRC-ANTCP             PIC S9(3)V9(3) COMP-3.
           10  16-TMIN-VCTO-TITLO              PIC S9(5)V COMP-3.
           10  16-TMAX-VCTO-TITLO              PIC S9(5)V COMP-3.
           10  16-CINDCD-PRZ-VCTO              PIC X(2).
           10  16-CREGRA-GERC-MORA             PIC S9(3)V COMP-3.
           10  16-CREGRA-GERC-CRATS            PIC S9(3)V COMP-3.
           10  16-CINDCD-TX-SPROD              PIC X(1).
           10  16-CTPO-TX-JURO-DESC            PIC S9(3)V COMP-3.
           10  16-CIDTFD-TX-JURO               PIC S9(5)V COMP-3.
           10  16-PINDIC-ECONM-JURO            PIC S9(3)V9(3) COMP-3.
           10  16-PTX-INDIC-ECONM              PIC S9(3)V9(3) COMP-3.
           10  16-HSIT-DESC-COML               PIC X(26).
           10  16-DULT-UTILZ-CONVE             PIC X(10).
           10  16-DULT-ADTTO-CONVE             PIC X(10).
           10  16-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  16-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  16-HULT-ATULZ                   PIC X(26).
           10  16-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  16-CTERM                        PIC X(8).
           10  16-PTAC-FIXA-CONVE              PIC 9(3)V99.
           10  16-CINDCD-PRZ-MAX               PIC X(2).
           10  16-QDIA-OPER-ATIVO              PIC S9(4)V COMP-3.
           10  16-CINDCD-FREQ-ESTOQ            PIC X(2).
           10  16-CINDCD-OPER-ATRSO            PIC X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  16-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  16-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  16-RETORNO.
       05  16-COD-RETORNO                         PIC 9(04).
       05  16-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  16-PROXIMO-RESTART                     PIC 9(05).
       05  16-QTDE-RETORNADA                      PIC 9(03).
       05  16-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  16-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T16.
           10  16RT-CCONVE-GRAL-DESC           PIC S9(9)V COMP-3.
           10  16RT-CVRSAO-CONVE-GRAL          PIC S9(3)V COMP-3.
           10  16RT-ICONVE-GRAL-DESC           PIC X(40).
           10  16RT-IRSUMO-CONVE-GRAL          PIC X(15).
           10  16RT-CBCO-OPER-CONVE            PIC S9(3)V COMP-3.
           10  16RT-CPRODT                     PIC S9(3)V COMP-3.
           10  16RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  16RT-CTPO-DEPDC-EXCVD           PIC X(1).
           10  16RT-CINDCD-ABRGC-GEOGR         PIC X(1).
           10  16RT-DINCL-CONVE-GRAL           PIC X(10).
           10  16RT-DINIC-VGCIA-CONVE          PIC X(10).
           10  16RT-DVCTO-CONVE-DESC           PIC X(10).
           10  16RT-TCONVE-DESC-COML           PIC S9(5)V COMP-3.
           10  16RT-CINDCD-PRZ-CONVE           PIC X(2).
           10  16RT-VLIM-CONVE-GRAL            PIC S9(15)V9(2) COMP-3.
           10  16RT-VUTLZD-CONVE-GRAL          PIC S9(15)V9(2) COMP-3.
           10  16RT-VDISPN-CONVE-GRAL          PIC S9(15)V9(2) COMP-3.
           10  16RT-HATULZ-SDO-CONVE           PIC X(26).
           10  16RT-CINDCD-CONVE-ROTVO         PIC X(1).
           10  16RT-CTPO-COBR-TAC              PIC S9(3)V COMP-3.
           10  16RT-CIDTFD-FIXO-TAC            PIC S9(5)V COMP-3.
           10  16RT-CIDTFD-PERC-TAC            PIC S9(5)V COMP-3.
           10  16RT-CIDTFD-MIN-TAC             PIC S9(5)V COMP-3.
           10  16RT-CIDTFD-MAX-TAC             PIC S9(5)V COMP-3.
           10  16RT-TDEB-PCELA-VENCD           PIC S9(3)V COMP-3.
           10  16RT-CINDCD-PRZ-PCELA           PIC X(2).
           10  16RT-CINDCD-TARIF-REG           PIC X(1).
           10  16RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  16RT-CINDCD-LIBRC-ANTCP         PIC X(1).
           10  16RT-PMAX-LIBRC-ANTCP           PIC S9(3)V9(3) COMP-3.
           10  16RT-TMIN-VCTO-TITLO            PIC S9(5)V COMP-3.
           10  16RT-TMAX-VCTO-TITLO            PIC S9(5)V COMP-3.
           10  16RT-CINDCD-PRZ-VCTO            PIC X(2).
           10  16RT-CREGRA-GERC-MORA           PIC S9(3)V COMP-3.
           10  16RT-CREGRA-GERC-CRATS          PIC S9(3)V COMP-3.
           10  16RT-CINDCD-TX-SPROD            PIC X(1).
           10  16RT-CTPO-TX-JURO-DESC          PIC S9(3)V COMP-3.
           10  16RT-CIDTFD-TX-JURO             PIC S9(5)V COMP-3.
           10  16RT-PINDIC-ECONM-JURO          PIC S9(3)V9(3) COMP-3.
           10  16RT-PTX-INDIC-ECONM            PIC S9(3)V9(3) COMP-3.
           10  16RT-HSIT-DESC-COML             PIC X(26).
           10  16RT-DULT-UTILZ-CONVE           PIC X(10).
           10  16RT-DULT-ADTTO-CONVE           PIC X(10).
           10  16RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  16RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  16RT-HULT-ATULZ                 PIC X(26).
           10  16RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  16RT-CTERM                      PIC X(8).
           10  16RT-PTAC-FIXA-CONVE            PIC 9(3)V99.
           10  16RT-CINDCD-PRZ-MAX             PIC X(2).
           10  16RT-QDIA-OPER-ATIVO            PIC S9(4)V COMP-3.
           10  16RT-CINDCD-FREQ-ESTOQ          PIC X(2).
           10  16RT-CINDCD-OPER-ATRSO          PIC X(1).

