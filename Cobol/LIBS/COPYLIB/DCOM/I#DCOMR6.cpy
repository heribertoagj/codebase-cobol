      ******************************************************************
      * SISTEMA : DCOM                                                 *
      * TAMANHO : 0700 BYTES                                           *
      * LAYOUT  : AREA DE TRABALHO PARA OS ADITAMENTOS                 *
      ******************************************************************
       01  DCOMR6-REG.

           03 R6-ADI-AGENCIA-CONTA.
              05  R6-CAG-BCRIA-DE             PIC  S9(05)      COMP-3.
              05  R6-CCTA-BCRIA-CLI-DE        PIC  S9(13)      COMP-3.
              05  R6-FILLER                   PIC   X(240).
              05  R6-CAG-BCRIA-PARA           PIC  S9(05)      COMP-3.
              05  R6-CCTA-BCRIA-CLI-PARA      PIC  S9(13)      COMP-3.
              05  FILLER                      PIC   X(240).

           03 R6-ADI-NRO-CONE      REDEFINES  R6-ADI-AGENCIA-CONTA.
              05  R6-CCART-DE                 PIC   X(05).
              05  R6-DANO-BASE-DE             PIC  S9(04)      COMP-3.
              05  R6-CDEPDC-DE                PIC  S9(05)      COMP-3.
              05  R6-CSEQ-STUDO-DE            PIC  S9(09)      COMP-3.
              05  R6-FILLER                   PIC   X(234).
              05  R6-CCART-PARA               PIC   X(05).
              05  R6-DANO-BASE-PARA           PIC  S9(04)      COMP-3.
              05  R6-CDEPDC-PARA              PIC  S9(05)      COMP-3.
              05  R6-CSEQ-STUDO-PARA          PIC  S9(09)      COMP-3.
              05  FILLER                      PIC   X(234).

           03 R6-ADI-TPO-GARANTIA  REDEFINES  R6-ADI-AGENCIA-CONTA.
              05  R6-CTPO-GARNT-DE-1          PIC  S9(03)      COMP-3.
              05  R6-CTPO-GARNT-DE-2          PIC  S9(03)      COMP-3.
              05  R6-FILLER                   PIC   X(246).
              05  R6-CTPO-GARNT-PARA-1        PIC  S9(03)      COMP-3.
              05  R6-CTPO-GARNT-PARA-2        PIC  S9(03)      COMP-3.
              05  R6-FILLER                   PIC   X(246).

           03 R6-ADI-TESTEMUNHA    REDEFINES  R6-ADI-AGENCIA-CONTA.
              05  R6-IPSSOA-DE                PIC   X(60).
              05  R6-CCNPJ-CPF-DE             PIC  S9(09)      COMP-3.
              05  R6-CFLIAL-CNPJ-DE           PIC  S9(05).
              05  R6-CCTRL-CNPJ-CPF-DE        PIC  S9(02)      COMP-3.
              05  R6-FILLER                   PIC   X(180).
              05  R6-IPSSOA-PARA              PIC   X(60).
              05  R6-CCNPJ-CPF-PARA           PIC  S9(09)      COMP-3.
              05  R6-CFLIAL-CNPJ-PARA         PIC  S9(05).
              05  R6-CCTRL-CNPJ-CPF-PARA      PIC  S9(02)      COMP-3.
              05  R6-FILLER                   PIC   X(176).

           03 R6-ADI-AVALISTA     REDEFINES   R6-ADI-AGENCIA-CONTA.
              05  R6-CCNPJ-CPF-AVAL-DE        PIC  S9(09)      COMP-3.
              05  R6-CFLIAL-CNPJ-AVAL-DE      PIC  S9(05)      COMP-3.
              05  R6-CCTRL-AVAL-DE            PIC  S9(02)      COMP-3.
              05  R6-IPSSOA-AVAL-DE           PIC   X(40).
              05  R6-ELOGDR-DE                PIC   X(40).
              05  R6-IBAIRO-DE                PIC   X(40).
              05  R6-IMUN-DE                  PIC   X(30).
              05  R6-CCEP-DE                  PIC  S9(05)      COMP-3.
              05  R6-CCEP-COMPL-DE            PIC  S9(03)      COMP-3.
              05  R6-CSGL-UF-DE               PIC   X(02).
              05  R6-CDOCTO-ID-AVAL-DE        PIC   X(15).
              05  R6-CINDCD-CONJG-AUTRZ-DE    PIC   X(01).
              05  R6-IPSSOA-CONJG-DE          PIC   X(40).
              05  R6-CPF-CONJG-DE             PIC  S9(09)      COMP-3.
              05  R6-CCTRL-CPF-CONJG-DE       PIC  S9(02)      COMP-3.
              05  R6-CDOCTO-ID-CONJG-DE       PIC   X(15).
              05  R6-NRELAC-PSSOA-DE          PIC  S9(09)      COMP-3.
              05  R6-CCNPJ-CPF-AVAL-PARA      PIC  S9(09)      COMP-3.
              05  R6-CFLIAL-AVAL-PARA         PIC  S9(05)      COMP-3.
              05  R6-CCTRL-AVAL-PARA          PIC  S9(02)      COMP-3.
              05  R6-IPSSOA-AVAL-PARA         PIC   X(40).
              05  R6-ELOGDR-PARA              PIC   X(40).
              05  R6-IBAIRO-PARA              PIC   X(40).
              05  R6-IMUN-PARA                PIC   X(30).
              05  R6-CCEP-PARA                PIC  S9(05)      COMP-3.
              05  R6-CCEP-COMPL-PARA          PIC  S9(03)      COMP-3.
              05  R6-CSGL-UF-PARA             PIC   X(02).
              05  R6-CDOCTO-ID-AVAL-PARA      PIC   X(15).
              05  R6-CINDCD-CONJG-AUTRZ-PARA  PIC   X(01).
              05  R6-IPSSOA-CONJG-PARA        PIC   X(40).
              05  R6-CPF-CONJG-PARA           PIC  S9(09)      COMP-3.
              05  R6-CCTRL-CPF-CONJG-PARA     PIC  S9(02)      COMP-3.
              05  R6-CDOCTO-ID-CONJG-PARA     PIC   X(15).
              05  R6-NRELAC-PSSOA-PARA        PIC  S9(09)      COMP-3.

           03 R6-ADI-TAXA-OPERACAO REDEFINES  R6-ADI-AGENCIA-CONTA.
              05  R6-PERC-TAXA-MES-DE         PIC  S9(03)V9(5) COMP-3.
              05  R6-PERC-TAXA-ANO-DE         PIC  S9(03)V9(5) COMP-3.
              05  R6-VALOR-JUROS-DE           PIC  S9(15)V9(2) COMP-3.
              05  R6-CAUTRZ-TX-TESOU-DE       PIC  S9(09)      COMP-3.
              05  R6-IAUTRZ-TX-TESOU-DE       PIC   X(40).
              05  R6-DESCR-TIPO-TAXA-DE       PIC   X(20).
              05  R6-VALOR-IOF-DE             PIC  S9(07)V9(2) COMP-3.
              05  R6-FILLER                   PIC   X(157).
              05  R6-PERC-TAXA-MES-PARA       PIC  S9(03)V9(5) COMP-3.
              05  R6-PERC-TAXA-ANO-PARA       PIC  S9(03)V9(5) COMP-3.
              05  R6-VALOR-JUROS-PARA         PIC  S9(15)V9(2) COMP-3.
              05  R6-CAUTRZ-TX-TESOU-PARA     PIC  S9(09)      COMP-3.
              05  R6-IAUTRZ-TX-TESOU-PARA     PIC   X(40).
              05  R6-DESCR-TIPO-TAXA-PARA     PIC   X(20).
              05  R6-VALOR-IOF-PARA           PIC  S9(07)V9(2) COMP-3.
              05  R6-FILLER                   PIC   X(157).
              05  R6-PMIN-TX-JURO-REGRA       PIC  S9(03)V9(3) COMP-3.
              05  R6-PMAX-TX-JURO-REGRA       PIC  S9(03)V9(3) COMP-3.

           03 R6-ADI-OPTIN         REDEFINES  R6-ADI-AGENCIA-CONTA.
              05  R6-FORMA-LIQDC-DE           PIC   X(01).
              05  R6-UTILZ-LIM-DE             PIC   X(01).
              05  R6-DEB-PCIAL-DE             PIC   X(01).
              05  R6-TENTV-DEB-DE             PIC   X(01).
              05  R6-FILLER                   PIC   X(246).
              05  R6-FORMA-LIQDC-PARA         PIC   X(01).
              05  R6-UTILZ-LIM-PARA           PIC   X(01).
              05  R6-DEB-PCIAL-PARA           PIC   X(01).
              05  R6-TENTV-DEB-PARA           PIC   X(01).
              05  R6-FILLER                   PIC   X(246).

           03 R6-ADI-TITULOS       REDEFINES  R6-ADI-AGENCIA-CONTA.
              05  R6-DEMIS-TITLO-DE           PIC   X(10).
              05  R6-DVCTO-TITLO-DESC-DE      PIC   X(10).
              05  R6-VTITLO-DESC-COML-DE      PIC  S9(15)V9(2) COMP-3.
              05  R6-CSEU-NRO-TITLO-DE        PIC   X(15).
              05  R6-CCNPJ-CPF-DE             PIC  S9(09)      COMP-3.
              05  R6-CFLIAL-CNPJ-DE           PIC  S9(05)      COMP-3.
              05  R6-CCTRL-CNPJ-DE            PIC  S9(02)      COMP-3.
              05  R6-IPSSOA-SACDO-DE          PIC   X(40).
              05  R6-CCEP-DE                  PIC  S9(05)      COMP-3.
              05  R6-CCEP-COMPL-DE            PIC  S9(03)      COMP-3.
              05  R6-ELOGDR-DE                PIC   X(40).
              05  R6-IBAIRO-DE                PIC   X(30).
              05  R6-IMUN-DE                  PIC   X(30).
              05  R6-CSGL-UF-DE               PIC   X(02).
              05  R6-VJURO-PCELA-DESC-DE      PIC  S9(15)V9(2) COMP-3.
              05  R6-VJURO-DIA-PCELA-DE       PIC  S9(15)V9(2) COMP-3.
              05  R6-VIOF-PCELA-DESC-DE       PIC  S9(15)V9(2) COMP-3.
              05  R6-VCUSTO-PCELA-DESC-DE     PIC  S9(15)V9(2) COMP-3.
              05  R6-VCUSTO-DIA-DE            PIC  S9(15)V9(2) COMP-3.
              05  R6-FILLER                   PIC   X(04).
              05  R6-DEMIS-TITLO-PARA         PIC   X(10).
              05  R6-DVCTO-TITLO-DESC-PARA    PIC   X(10).
              05  R6-VTITLO-DESC-COML-PARA    PIC  S9(15)V9(2) COMP-3.
              05  R6-CSEU-NRO-TITLO-PARA      PIC   X(15).
              05  R6-CCNPJ-CPF-PARA           PIC  S9(09)      COMP-3.
              05  R6-CFLIAL-CNPJ-PARA         PIC  S9(05)      COMP-3.
              05  R6-CCTRL-CNPJ-PARA          PIC  S9(02)      COMP-3.
              05  R6-IPSSOA-SACDO-PARA        PIC   X(40).
              05  R6-CCEP-PARA                PIC  S9(05)      COMP-3.
              05  R6-CCEP-COMPL-PARA          PIC  S9(03)      COMP-3.
              05  R6-ELOGDR-PARA              PIC   X(40).
              05  R6-IBAIRO-PARA              PIC   X(30).
              05  R6-IMUN-PARA                PIC   X(30).
              05  R6-CSGL-UF-PARA             PIC   X(02).
              05  R6-VJURO-PCELA-DESC-PARA    PIC  S9(15)V9(2) COMP-3.
              05  R6-VJURO-DIA-PCELA-PARA     PIC  S9(15)V9(2) COMP-3.
              05  R6-VIOF-PCELA-DESC-PARA     PIC  S9(15)V9(2) COMP-3.
              05  R6-VCUSTO-PCELA-DESC-PARA   PIC  S9(15)V9(2) COMP-3.
              05  R6-VCUSTO-DIA-PARA          PIC  S9(15)V9(2) COMP-3.
              05  R6-FILLER                   PIC   X(04).

           03 R6-ADI-INSTRUCAO    REDEFINES   R6-ADI-AGENCIA-CONTA.
              05  R6-DINSTR-DESC-PCELA1-DE    PIC   X(10).
              05  R6-VINSTR-DESC-PCELA1-DE    PIC  S9(15)V9(2) COMP-3.
              05  R6-PINSTR-DESC-PCELA1-DE    PIC  S9(03)V9(3) COMP-3.
              05  R6-DINSTR-DESC-PCELA2-DE    PIC   X(10).
              05  R6-VINSTR-DESC-PCELA2-DE    PIC  S9(15)V9(2) COMP-3.
              05  R6-PINSTR-DESC-PCELA2-DE    PIC  S9(03)V9(3) COMP-3.
              05  R6-FILLER                   PIC   X(227).
              05  R6-DINSTR-DESC-PCELA1-PARA  PIC   X(10).
              05  R6-VINSTR-DESC-PCELA1-PARA  PIC  S9(15)V9(2) COMP-3.
              05  R6-PINSTR-DESC-PCELA1-PARA  PIC  S9(03)V9(3) COMP-3.
              05  R6-DINSTR-DESC-PCELA2-PARA  PIC   X(10).
              05  R6-VINSTR-DESC-PCELA2-PARA  PIC  S9(15)V9(2) COMP-3.
              05  R6-PINSTR-DESC-PCELA2-PARA  PIC  S9(03)V9(3) COMP-3.
              05  R6-FILLER                   PIC   X(181).

           03 R6-ADI-SAC-AVAL      REDEFINES  R6-ADI-AGENCIA-CONTA.
              05  R6-CINDCD-PSSOA-DESC-DE     PIC   X(02).
              05  R6-CELMTO-DESC-COML-DE      PIC  S9(03)      COMP-3.
              05  R6-CCHAVE-ELMTO-DESC-DE     PIC   X(45).
              05  R6-CCNPJ-SAC-AVAL-DE        PIC  S9(09)      COMP-3.
              05  R6-CFLIAL-SAC-AVAL-DE       PIC  S9(05)      COMP-3.
              05  R6-CCTRL-SAC-AVAL-DE        PIC  S9(02)      COMP-3.
              05  R6-IPSSOA-SAC-AVAL-DE       PIC   X(40).
              05  R6-NPSSOA-SAC-AVAL-DE       PIC   9(09).
              05  R6-FILLER                   PIC   X(142).
              05  R6-CINDCD-PSSOA-DESC-PARA   PIC   X(02).
              05  R6-CELMTO-DESC-COML-PARA    PIC  S9(03)      COMP-3.
              05  R6-CCHAVE-ELMTO-DESC-PARA   PIC   X(45).
              05  R6-CCNPJ-SAC-AVAL-PARA      PIC  S9(09)      COMP-3.
              05  R6-CFLIAL-SAC-AVAL-PARA     PIC  S9(05)      COMP-3.
              05  R6-CCTRL-SAC-AVAL-PARA      PIC  S9(02)      COMP-3.
              05  R6-IPSSOA-SAC-AVAL-PARA     PIC   X(40).
              05  R6-NPSSOA-SAC-AVAL-PARA     PIC   9(09).
              05  R6-FILLER                   PIC   X(142).

           03 R6-ADI-VALOR-OPER    REDEFINES  R6-ADI-AGENCIA-CONTA.
              05  R6-VOPER-DESC-COML-DE       PIC  S9(15)V9(2) COMP-3.
              05  R6-VLIQ-OPER-DESC-DE        PIC  S9(15)V9(2) COMP-3.
              05  R6-VJURO-OPER-ADIT-DE       PIC  S9(15)V9(2) COMP-3.
              05  R6-VIOF-OPER-ADIT-DE        PIC  S9(15)V9(2) COMP-3.
              05  R6-PTX-JURO-MES-ADIT-DE     PIC  S9(03)V9(5) COMP-3.
              05  R6-VOPER-DESC-APURA-DE      PIC  S9(15)V9(2) COMP-3.
              05  R6-DT-ULT-ADIT-DE           PIC   X(10).
              05  R6-FILLER                   PIC   X(190).
              05  R6-VOPER-DESC-COML-PARA     PIC  S9(15)V9(2) COMP-3.
              05  R6-VLIQ-OPER-DESC-PARA      PIC  S9(15)V9(2) COMP-3.
              05  R6-VJURO-OPER-ADIT-PARA     PIC  S9(15)V9(2) COMP-3.
              05  R6-VIOF-OPER-ADIT-PARA      PIC  S9(15)V9(2) COMP-3.
              05  R6-PTX-JURO-MES-ADIT-PARA   PIC  S9(03)V9(5) COMP-3.
              05  R6-VOPER-DESC-APURA-PARA    PIC  S9(15)V9(2) COMP-3.
              05  R6-DT-ULT-ADIT-PARA         PIC   X(10).
              05  R6-FILLER                   PIC   X(190).

           03 R6-PRORROGA-PARCELA  REDEFINES  R6-ADI-AGENCIA-CONTA.
              05  R6-DVCTO-PCELA-DESC-DE      PIC   X(10).
              05  R6-PTX-JURO-MES-DE          PIC  S9(03)V9(5) COMP-3.
              05  R6-VJURO-PRROG-PCELA-DE     PIC  S9(15)V9(2) COMP-3.
              05  R6-PIOF-PRROG-PCELA-DE      PIC  S9(03)V9(5) COMP-3.
              05  R6-VIOF-PRROG-PCELA-DE      PIC  S9(15)V9(2) COMP-3.
              05  R6-VTAXA-PRROG-PCELA-DE     PIC  S9(15)V9(2) COMP-3.
              05  R6-VVLOR-PCELA-DE           PIC  S9(15)V9(2) COMP-3.
              05  R6-TFREQ-TAXA-PCELA-DE      PIC   X(04).
              05  R6-VJURO-DIA-PRROG-DE       PIC  S9(15)V9(2) COMP-3.
              05  R6-VCUSTO-PRROG-PCELA-DE    PIC  S9(15)V9(2) COMP-3.
              05  R6-FILLER                   PIC   X(172).
              05  R6-DVCTO-PCELA-DESC-PARA    PIC   X(10).
              05  R6-PTX-JURO-MES-PARA        PIC  S9(03)V9(5) COMP-3.
              05  R6-VJURO-PRROG-PCELA-PARA   PIC  S9(15)V9(2) COMP-3.
              05  R6-PIOF-PRROG-PCELA-PARA    PIC  S9(03)V9(5) COMP-3.
              05  R6-VIOF-PRROG-PCELA-PARA    PIC  S9(15)V9(2) COMP-3.
              05  R6-VTAXA-PRROG-PCELA-PARA   PIC  S9(15)V9(2) COMP-3.
              05  R6-TFREQ-TAXA-PCELA-PARA    PIC   X(04).
              05  R6-VJURO-DIA-PRROG-PARA     PIC  S9(15)V9(2) COMP-3.
              05  R6-PMIN-TAXA-JURO-PARA      PIC  S9(03)V9(3) COMP-3.
              05  R6-PMAX-TAXA-JURO-PARA      PIC  S9(03)V9(3) COMP-3.
              05  R6-CTPO-TX-JURO             PIC   9(03).
              05  R6-ITPO-TX-JURO             PIC   X(15).
              05  R6-VCUSTO-PRROG-PCELA-PARA  PIC  S9(15)V9(2) COMP-3.
              05  R6-FILLER                   PIC   X(155).

           03 R6-DADOS-GERAIS.
              05  R6-CPRODT                   PIC  S9(03)      COMP-3.
              05  R6-CSPROD-DESC-COML         PIC  S9(03)      COMP-3.
              05  R6-CBCO-OPER-OPER           PIC  S9(03)      COMP-3.
              05  R6-CAG-OPER-OPER            PIC  S9(05)      COMP-3.
              05  R6-CTPO-AUTRZ-ESPCL         PIC  S9(03)      COMP-3.
              05  R6-CAUTRZ-ESPCL             PIC  S9(09)      COMP-3.
              05  R6-IAUTRZ-ESPCL             PIC   X(40).
              05  R6-CIND-COBR-TAC            PIC   X(01).
              05  R6-CINDCD-FREQ-TX           PIC   X(02).
              05  R6-FILLER                   PIC   X(41).

           03 R6-SENSIBILIZA-CONE.
              05  R6-TITULO-RECUSADO-ANLSE    PIC   X(01).
              05  R6-CELMTO-DESC-COML         PIC   9(03) COMP-3.
              05  R6-CCONTR-CONVE-DESC        PIC   9(09) COMP-3.
              05  R6-CVRSAO-CONTR-CONVE       PIC   9(03) COMP-3.
              05  R6-DANO-BASE                PIC   9(04) COMP-3.
              05  R6-CDEPDC                   PIC   9(05) COMP-3.
              05  R6-CSEQ-STUDO               PIC   9(09) COMP-3.
              05  R6-CCART-NORML-DESC         PIC   X(05).
              05  R6-CCNPJ-CPF                PIC   9(09) COMP-3.
              05  R6-CFLIAL-CNPJ              PIC   9(05) COMP-3.
              05  R6-CCTRL-CNPJ-CPF           PIC   9(02) COMP-3.
              05  R6-FILLER                   PIC   X(14).

           03 R6-CUSTO-EFETIVO-TOTAL.
              05  R6-CET-DIA-DE               PIC  S9(08)V9(07) COMP-3.
              05  R6-CET-MES-DE               PIC  S9(08)V9(07) COMP-3.
              05  R6-CET-ANO-DE               PIC  S9(08)V9(07) COMP-3.
              05  R6-CET-DIA-PARA             PIC  S9(08)V9(07) COMP-3.
              05  R6-CET-MES-PARA             PIC  S9(08)V9(07) COMP-3.
              05  R6-CET-ANO-PARA             PIC  S9(08)V9(07) COMP-3.
              05  FILLER                      PIC   X(02).

