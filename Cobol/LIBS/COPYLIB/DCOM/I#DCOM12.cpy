      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB012 - DB2PRD.TCONVE_CLI_DESC        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  12-ARGUMENTOS-ENTRADA.

       05  12-COLUNAS-DA-TABELA.
           10  12-CCONVE-CLI-DESC              PIC S9(9)V COMP-3.
           10  12-CVRSAO-CONVE-CLI             PIC S9(3)V COMP-3.
           10  12-CBCO                         PIC S9(3)V COMP-3.
           10  12-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  12-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  12-CPOSTO-SERVC                 PIC S9(5)V COMP-3.
           10  12-CBCO-OPER-CONVE              PIC S9(3)V COMP-3.
           10  12-CAG-OPER-CONVE               PIC S9(5)V COMP-3.
           10  12-CINDCD-RENTB-NEGOC           PIC X(1).
           10  12-CPRODT                       PIC S9(3)V COMP-3.
           10  12-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  12-DINCL-CONVE-CLI              PIC X(10).
           10  12-DINIC-VGCIA-CONVE            PIC X(10).
           10  12-DVCTO-CONVE-DESC             PIC X(10).
           10  12-TCONVE-DESC-COML             PIC S9(5)V COMP-3.
           10  12-CINDCD-PRZ-CONVE             PIC X(2).
           10  12-VLIM-CONVE-CLI               PIC S9(15)V9(2) COMP-3.
           10  12-VUTLZD-CONVE-CLI             PIC S9(15)V9(2) COMP-3.
           10  12-VDISPN-CONVE-CLI             PIC S9(15)V9(2) COMP-3.
           10  12-HATULZ-SDO-CONVE             PIC X(26).
           10  12-CINDCD-CONVE-ROTVO           PIC X(1).
           10  12-CTPO-COBR-TAC                PIC S9(3)V COMP-3.
           10  12-CIDTFD-FIXO-TAC              PIC S9(5)V COMP-3.
           10  12-CIDTFD-PERC-TAC              PIC S9(5)V COMP-3.
           10  12-CIDTFD-MIN-TAC               PIC S9(5)V COMP-3.
           10  12-CIDTFD-MAX-TAC               PIC S9(5)V COMP-3.
           10  12-CINDCD-TARIF-REG             PIC X(1).
           10  12-CINDCD-PGDOR-JURO            PIC X(1).
           10  12-CINDCD-RTEIO-BNEFC           PIC X(1).
           10  12-CINDCD-LIBRC-ANTCP           PIC X(1).
           10  12-PMAX-LIBRC-ANTCP             PIC S9(3)V9(3) COMP-3.
           10  12-PMAX-ANTVL-VCTO              PIC S9(3)V9(3) COMP-3.
           10  12-TMIN-VCTO-TITLO              PIC S9(5)V COMP-3.
           10  12-TMAX-VCTO-TITLO              PIC S9(5)V COMP-3.
           10  12-CINDCD-PRZ-VCTO              PIC X(2).
           10  12-CREGRA-GERC-MORA             PIC S9(3)V COMP-3.
           10  12-CREGRA-GERC-CRATS            PIC S9(3)V COMP-3.
           10  12-CINDCD-TX-SPROD              PIC X(1).
           10  12-CTPO-TX-JURO-DESC            PIC S9(3)V COMP-3.
           10  12-CIDTFD-TX-JURO               PIC S9(5)V COMP-3.
           10  12-PINDIC-ECONM-JURO            PIC S9(3)V9(3) COMP-3.
           10  12-PTX-INDIC-ECONM              PIC S9(3)V9(3) COMP-3.
           10  12-CREGRA-COMIS-CONVE           PIC S9(3)V COMP-3.
           10  12-VCALC-COMIS-CONVE            PIC S9(15)V9(2) COMP-3.
>          10  12-PCALC-COMIS-CONVE            PIC S9(3)V9(2) COMP-3.
           10  12-DDIA-PGTO-COMIS              PIC S9(2)V COMP-3.
           10  12-CINDCD-CONVE-ESPCL           PIC X(1).
           10  12-TRECPC-ARQ-CONVE             PIC S9(3)V COMP-3.
           10  12-CINDCD-PRZ-RECPC             PIC X(2).
           10  12-TARQ-DEB-CONVE               PIC S9(3)V COMP-3.
           10  12-CINDCD-PRZ-ARQ-DEB           PIC X(2).
           10  12-CINDCD-CONSL-DEB             PIC X(1).
           10  12-CINDCD-CONSL-EXTRT           PIC X(1).
           10  12-CINDCD-MEIO-COMUN            PIC X(1).
           10  12-CDDD-FAX-CONVN               PIC X(4).
           10  12-CFONE-FAX-CONVN              PIC S9(8)V COMP-3.
           10  12-CRMAL-FAX-CONVN              PIC X(6).
           10  12-EEMAIL-CLI-CONVN             PIC X(70).
           10  12-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  12-HSIT-DESC-COML               PIC X(26).
           10  12-DULT-UTILZ-CONVE             PIC X(10).
           10  12-DULT-ADTTO-CONVE             PIC X(10).
           10  12-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  12-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  12-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  12-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  12-CCLUB                        PIC S9(10)V COMP-3.
           10  12-HULT-ATULZ                   PIC X(26).
           10  12-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  12-CTERM                        PIC X(8).
           10  12-CINDCD-PROTE-TITLO           PIC X(1).
           10  12-QDIA-PROTE-TITLO             PIC S9(3)V COMP-3.
           10  12-CCHAVE-ELMTO-DESC            PIC X(45).
           10  12-PTAC-FIXA-CONVE              PIC 9(5).
           10  12-CINDCD-PRZ-MAX               PIC X(2).
           10  12-CINDCD-DIA-PROTE             PIC X(2).
           10  12-QDIA-OPER-ATIVO              PIC S9(4)V COMP-3.
           10  12-CINDCD-FREQ-ESTOQ            PIC X(2).
           10  12-CINDCD-OPER-ATRSO            PIC X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  12-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  12-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  12-RETORNO.
       05  12-COD-RETORNO                         PIC 9(04).
       05  12-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  12-PROXIMO-RESTART                     PIC 9(05).
       05  12-QTDE-RETORNADA                      PIC 9(03).
       05  12-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  12-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T12.
           10  12RT-CCONVE-CLI-DESC            PIC S9(9)V COMP-3.
           10  12RT-CVRSAO-CONVE-CLI           PIC S9(3)V COMP-3.
           10  12RT-CBCO                       PIC S9(3)V COMP-3.
           10  12RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  12RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  12RT-CPOSTO-SERVC               PIC S9(5)V COMP-3.
           10  12RT-CBCO-OPER-CONVE            PIC S9(3)V COMP-3.
           10  12RT-CAG-OPER-CONVE             PIC S9(5)V COMP-3.
           10  12RT-CINDCD-RENTB-NEGOC         PIC X(1).
           10  12RT-CPRODT                     PIC S9(3)V COMP-3.
           10  12RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  12RT-DINCL-CONVE-CLI            PIC X(10).
           10  12RT-DINIC-VGCIA-CONVE          PIC X(10).
           10  12RT-DVCTO-CONVE-DESC           PIC X(10).
           10  12RT-TCONVE-DESC-COML           PIC S9(5)V COMP-3.
           10  12RT-CINDCD-PRZ-CONVE           PIC X(2).
           10  12RT-VLIM-CONVE-CLI             PIC S9(15)V9(2) COMP-3.
           10  12RT-VUTLZD-CONVE-CLI           PIC S9(15)V9(2) COMP-3.
           10  12RT-VDISPN-CONVE-CLI           PIC S9(15)V9(2) COMP-3.
           10  12RT-HATULZ-SDO-CONVE           PIC X(26).
           10  12RT-CINDCD-CONVE-ROTVO         PIC X(1).
           10  12RT-CTPO-COBR-TAC              PIC S9(3)V COMP-3.
           10  12RT-CIDTFD-FIXO-TAC            PIC S9(5)V COMP-3.
           10  12RT-CIDTFD-PERC-TAC            PIC S9(5)V COMP-3.
           10  12RT-CIDTFD-MIN-TAC             PIC S9(5)V COMP-3.
           10  12RT-CIDTFD-MAX-TAC             PIC S9(5)V COMP-3.
           10  12RT-CINDCD-TARIF-REG           PIC X(1).
           10  12RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  12RT-CINDCD-RTEIO-BNEFC         PIC X(1).
           10  12RT-CINDCD-LIBRC-ANTCP         PIC X(1).
           10  12RT-PMAX-LIBRC-ANTCP           PIC S9(3)V9(3) COMP-3.
           10  12RT-PMAX-ANTVL-VCTO            PIC S9(3)V9(3) COMP-3.
           10  12RT-TMIN-VCTO-TITLO            PIC S9(5)V COMP-3.
           10  12RT-TMAX-VCTO-TITLO            PIC S9(5)V COMP-3.
           10  12RT-CINDCD-PRZ-VCTO            PIC X(2).
           10  12RT-CREGRA-GERC-MORA           PIC S9(3)V COMP-3.
           10  12RT-CREGRA-GERC-CRATS          PIC S9(3)V COMP-3.
           10  12RT-CINDCD-TX-SPROD            PIC X(1).
           10  12RT-CTPO-TX-JURO-DESC          PIC S9(3)V COMP-3.
           10  12RT-CIDTFD-TX-JURO             PIC S9(5)V COMP-3.
           10  12RT-PINDIC-ECONM-JURO          PIC S9(3)V9(3) COMP-3.
           10  12RT-PTX-INDIC-ECONM            PIC S9(3)V9(3) COMP-3.
           10  12RT-CREGRA-COMIS-CONVE         PIC S9(3)V COMP-3.
           10  12RT-VCALC-COMIS-CONVE          PIC S9(15)V9(2) COMP-3.
>          10  12RT-PCALC-COMIS-CONVE          PIC S9(3)V9(2) COMP-3.
           10  12RT-DDIA-PGTO-COMIS            PIC S9(2)V COMP-3.
           10  12RT-CINDCD-CONVE-ESPCL         PIC X(1).
           10  12RT-TRECPC-ARQ-CONVE           PIC S9(3)V COMP-3.
           10  12RT-CINDCD-PRZ-RECPC           PIC X(2).
           10  12RT-TARQ-DEB-CONVE             PIC S9(3)V COMP-3.
           10  12RT-CINDCD-PRZ-ARQ-DEB         PIC X(2).
           10  12RT-CINDCD-CONSL-DEB           PIC X(1).
           10  12RT-CINDCD-CONSL-EXTRT         PIC X(1).
           10  12RT-CINDCD-MEIO-COMUN          PIC X(1).
           10  12RT-CDDD-FAX-CONVN             PIC X(4).
           10  12RT-CFONE-FAX-CONVN            PIC S9(8)V COMP-3.
           10  12RT-CRMAL-FAX-CONVN            PIC X(6).
           10  12RT-EEMAIL-CLI-CONVN           PIC X(70).
           10  12RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  12RT-HSIT-DESC-COML             PIC X(26).
           10  12RT-DULT-UTILZ-CONVE           PIC X(10).
           10  12RT-DULT-ADTTO-CONVE           PIC X(10).
           10  12RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  12RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  12RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  12RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  12RT-CCLUB                      PIC S9(10)V COMP-3.
           10  12RT-HULT-ATULZ                 PIC X(26).
           10  12RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  12RT-CTERM                      PIC X(8).
           10  12RT-CINDCD-PROTE-TITLO         PIC X(1).
           10  12RT-QDIA-PROTE-TITLO           PIC S9(3)V COMP-3.
           10  12RT-CCHAVE-ELMTO-DESC          PIC X(45).
           10  12RT-PTAC-FIXA-CONVE            PIC 9(5).
           10  12RT-CINDCD-PRZ-MAX             PIC X(2).
           10  12RT-CINDCD-DIA-PROTE           PIC X(2).
           10  12RT-QDIA-OPER-ATIVO            PIC S9(4)V COMP-3.
           10  12RT-CINDCD-FREQ-ESTOQ          PIC X(2).
           10  12RT-CINDCD-OPER-ATRSO          PIC X(1).

