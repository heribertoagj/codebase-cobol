      ******************************************************************
      * NOME BOOK : I#DCOMZR                                           *
      * DESCRICAO : BOOK DE INTERFACE TPCELA_OPER_DESC                 *
      * DATA      : 11/2020                                            *
      * AUTOR     : AMANDA BELTOSO  - CAPGEMINI - SALVADOR             *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : DCOM - DESCONTO COMERCIAL                          *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL                          *
      * TAMANHO   : 0399 BYTES                                         *
      ******************************************************************
      *
       05  DCOMZR-REGISTRO.
           10 DCOMZR-C9-DANO-OPER-DESC         PIC S9(4)V COMP-3.
           10 DCOMZR-C9-NSEQ-OPER-DESC         PIC S9(9)V COMP-3.
           10 DCOMZR-C9-NPCELA-DESC-COML       PIC S9(5)V COMP-3.
           10 DCOMZR-C9-DMES-VCTO-PTCAO        PIC S9(2)V COMP-3.
           10 DCOMZR-C9-CSIT-DESC-COML         PIC S9(3)V COMP-3.
           10 DCOMZR-C9-HSIT-DESC-COML         PIC X(26).
           10 DCOMZR-C9-DVCTO-PCELA-DESC       PIC X(10).
           10 DCOMZR-C9-VPRINC-PCELA-DESC      PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-QTITLO-CHEQ-PCELA      PIC S9(9)V COMP-3.
           10 DCOMZR-C9-PTX-JURO-MES           PIC S9(3)V9(5) COMP-3.
           10 DCOMZR-C9-PTX-JURO-ANO           PIC S9(3)V9(5) COMP-3.
           10 DCOMZR-C9-PTX-CUSTO-MES          PIC S9(3)V9(6) COMP-3.
           10 DCOMZR-C9-PTX-CUSTO-ANO          PIC S9(3)V9(6) COMP-3.
           10 DCOMZR-C9-VCUSTO-PCELA-DESC      PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VJURO-PCELA-DESC       PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VJURO-DIA-PCELA        PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VIOF-PCELA-DESC        PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VPSENT-PCELA-DESC      PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VJURO-APROP-ACUM       PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-DULT-APROP-PCELA       PIC X(10).
           10 DCOMZR-C9-PALIQT-IOF-PRROG       PIC S9(3)V9(5) COMP-3.
           10 DCOMZR-C9-VIOF-PRROG-PCELA       PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-PCUSTO-MES-MORA        PIC S9(3)V9(6) COMP-3.
           10 DCOMZR-C9-PCUSTO-ANO-MORA        PIC S9(3)V9(6) COMP-3.
           10 DCOMZR-C9-PCUSTO-MES-CRATS       PIC S9(3)V9(6) COMP-3.
           10 DCOMZR-C9-PCUSTO-ANO-CRATS       PIC S9(3)V9(6) COMP-3.
           10 DCOMZR-C9-PALIQT-IOF-VENCD       PIC S9(3)V9(5) COMP-3.
           10 DCOMZR-C9-VIOF-PCELA-VENCD       PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VJURO-MORA-GERC        PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VJURO-MORA-LEGAL       PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VSDO-DVDOR-PRINC       PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VCUSTO-PCELA-VENCD     PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VJURO-TRNSF-CRATS      PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VCUSTO-TRNSF-CRATS     PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-DTRNSF-CRATS-PCELA     PIC X(10).
           10 DCOMZR-C9-DTRNSF-LP-PCELA        PIC X(10).
           10 DCOMZR-C9-CBACEN-DESC-RURAL      PIC S9(9)V COMP-3.
           10 DCOMZR-C9-CCONTR-CORP-PCELA      PIC S9(7)V COMP-3.
           10 DCOMZR-C9-DULT-ADTTO-PCELA       PIC X(10).
           10 DCOMZR-C9-CINDCD-MOTVO-ADTTO     PIC X(1).
           10 DCOMZR-C9-CINDCD-PEND-EXTER      PIC X(1).
           10 DCOMZR-C9-CINDCD-BLOQ-CONS       PIC X(1).
           10 DCOMZR-C9-DBLOQ-CONS-CCORR       PIC X(10).
           10 DCOMZR-C9-DULT-PRROG-PCELA       PIC X(10).
           10 DCOMZR-C9-DULT-REATV-PCELA       PIC X(10).
           10 DCOMZR-C9-HULT-ATULZ             PIC X(26).
           10 DCOMZR-C9-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10 DCOMZR-C9-CTERM                  PIC X(8).
           10 DCOMZR-C9-DINCL-REG              PIC X(10).
           10 DCOMZR-C9-VJURO-REMUN-VENCD      PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VMORA-VENCD-DESC       PIC S9(15)V9(2) COMP-3.
           10 DCOMZR-C9-VMULTA-VENCD-DESC      PIC S9(15)V9(2) COMP-3.
